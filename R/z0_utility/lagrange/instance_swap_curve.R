# 移植元: LagrangeFinance/R/instance_swap_curve.R（ブランチ refactor/gaussquant-consolidation のコミット 371d12d。e6614e1 からの差分は GaussQuant の日付・評価日関数への置き換えのみ）
# 関数名・引数・計算ロジックは変更していない。この行より下は移植元のまま。
# ------------------------------------------------------------------------
# Currency-dispatched OIS curves for compute_asset_swap_ISO().
# Input: observed spot OIS par quotes, NOT overnight fixings or IBOR swap rates.
# rate_pct is in percent (4.25 means 4.25%). No market data is fabricated.
# Supply quotes explicitly; retained TONAR sample providers live in the unified sample.

ois_currency_spec <- function(currency) {
  if (!is.character(currency) || length(currency) != 1L || is.na(currency)) {
    stop("currency must be one currency code.", call. = FALSE)
  }
  currency <- toupper(trimws(currency))
  registry <- list(
    JPY = list(index_name = "TONAR", constructor = "Tonar", settlement_days = 2L),
    USD = list(index_name = "SOFR", constructor = "Sofr", settlement_days = 2L),
    EUR = list(index_name = "ESTR", constructor = "Estr", settlement_days = 2L),
    GBP = list(index_name = "SONIA", constructor = "Sonia", settlement_days = 0L),
    AUD = list(index_name = "AONIA", constructor = "Aonia", settlement_days = 1L)
  )
  spec <- registry[[currency]]
  if (is.null(spec)) {
    stop("Unsupported currency: ", currency, ". Use JPY, USD, EUR, GBP or AUD.",
         call. = FALSE)
  }
  c(list(currency = currency), spec)
}

# A single dispatch table selects the QuantLib constructor, calendar and day count.
instance_overnight_index <- function(currency, curve_handle = NULL) {
  spec <- ois_currency_spec(currency)
  ns <- asNamespace("QuantLib")
  if (!exists(spec$constructor, ns, inherits = FALSE)) {
    stop("QuantLib R binding unavailable: ", spec$constructor, call. = FALSE)
  }
  constructor <- get(spec$constructor, ns, inherits = FALSE)
  if (is.null(curve_handle)) constructor() else constructor(curve_handle)
}


# One builder for JPY/USD/EUR/GBP/AUD.
# Canonical date column/argument: marketdate. date is a compatibility alias.
# Input must contain genuine OIS par quotes or an explicitly typed deposit.
instance_swap_curve <- function(
  currency, marketdate = NULL, quotes = NULL,
  curve_type = c("log_cubic_discount", "flat_forward"),
  settlement_days = NULL, payment_lag = 0L,
  payment_frequency = "Annual", extrapolate = FALSE, date = NULL
) {
  spec <- ois_currency_spec(currency)
  curve_type <- match.arg(curve_type)
  if (!is.null(date)) {
    if (!is.null(marketdate) &&
        !identical(as.Date(date), as.Date(marketdate))) stop("Conflicting dates.")
    marketdate <- date
  }
  if (is.null(marketdate)) stop("Supply marketdate.")
  marketdate <- as.Date(marketdate)
  if (length(marketdate) != 1L || is.na(marketdate)) stop("Choose one marketdate.")
  if (is.null(quotes)) {
    if (spec$currency != "JPY" || !exists("tonar_market_quotes", mode = "function")) {
      stop("Supply quotes for the requested currency and marketdate.")
    }
    quotes <- tonar_market_quotes(marketdate)
  }
  if (!is.data.frame(quotes) || !nrow(quotes)) stop("Supply nonempty quote data.")
  if (!"marketdate" %in% names(quotes) && "date" %in% names(quotes)) {
    quotes$marketdate <- as.Date(quotes$date)
  }
  if (!all(c("marketdate", "tenor", "rate_pct") %in% names(quotes))) {
    stop("quotes requires marketdate, tenor and rate_pct.")
  }
  if (!"currency" %in% names(quotes)) quotes$currency <- spec$currency
  if (anyNA(quotes$marketdate) || anyNA(quotes$currency)) stop("Missing quote keys.")
  q <- tibble::as_tibble(quotes) |>
    dplyr::filter(
      as.Date(.data$marketdate) == .env$marketdate,
      toupper(.data$currency) == .env$spec$currency
    )
  if (!nrow(q)) stop("No quotes for currency/marketdate.")
  if (!"instrument_type" %in% names(q)) q$instrument_type <- "spot_ois"
  # Historical overnight fixings must not masquerade as 1D OIS quotes.
  if (anyNA(q$instrument_type) ||
      any(!q$instrument_type %in% c("spot_ois", "deposit"))) {
    stop("Only spot_ois/deposit inputs allowed. Separate overnight fixings and unconfirmed IRS.")
  }
  q$tenor <- toupper(trimws(q$tenor))
  if (anyNA(q$tenor) || any(!grepl("^[1-9][0-9]*[DWMY]$", q$tenor)) ||
      anyDuplicated(q$tenor)) stop("Invalid/duplicate tenors.")
  if (!is.numeric(q$rate_pct) || any(!is.finite(q$rate_pct))) stop("Invalid rates.")
  q$marketdate <- as.Date(q$marketdate)
  q$currency <- spec$currency
  q$rate <- q$rate_pct / 100
  if (!"instrument" %in% names(q)) q$instrument <- paste0(spec$index_name, "_OIS")
  if (!"ticker" %in% names(q)) q$ticker <- NA_character_
  if (is.null(settlement_days)) settlement_days <- spec$settlement_days
  valid_lag <- function(x) is.numeric(x) && length(x) == 1L &&
    is.finite(x) && x >= 0 && x <= .Machine$integer.max && x == floor(x)
  if (!valid_lag(settlement_days) || !valid_lag(payment_lag)) stop("Invalid lags.")
  ns <- asNamespace("QuantLib")
  fname <- paste0("Frequency_", payment_frequency, "_get")
  if (length(fname) != 1L || !exists(fname, ns, inherits = FALSE)) stop("Invalid frequency.")
  frequency <- get(fname, ns, inherits = FALSE)()
  GaussQuant::set_eval_date_GQL(marketdate)
  index <- instance_overnight_index(spec$currency)
  calendar <- index$fixingCalendar()
  day_counter <- index$dayCounter()
  units <- c(D = "Days", W = "Weeks", M = "Months", Y = "Years")
  helpers <- purrr::pmap(
    list(q$tenor, q$rate, q$instrument_type),
    function(tenor, rate, kind) {
      handle <- QuantLib::QuoteHandle(QuantLib::SimpleQuote(rate))
      if (kind == "deposit") {
        if (tenor != "1D") stop("Only overnight index deposits are supported.")
        return(QuantLib::DepositRateHelper(handle, index))
      }
      period <- QuantLib::Period(
        as.integer(sub("[DWMY]$", "", tenor)), unname(units[substr(tenor, nchar(tenor), nchar(tenor))])
      )
      QuantLib::OISRateHelper(
        as.integer(settlement_days), period, handle, index,
        QuantLib::YieldTermStructureHandle(), FALSE,
        as.integer(payment_lag), "ModifiedFollowing", frequency
      )
    }
  )
  helper_vector <- QuantLib::RateHelperVector()
  purrr::walk(helpers, function(h) helper_vector$push_back(h))
  constructor <- switch(curve_type,
    log_cubic_discount = QuantLib::PiecewiseLogCubicDiscount,
    flat_forward = QuantLib::PiecewiseFlatForward
  )
  curve <- constructor(0L, calendar, helper_vector, day_counter)
  if (isTRUE(extrapolate)) QuantLib::TermStructure_enableExtrapolation(curve)
  terminal_df <- as.numeric(curve$discount(curve$maxDate()))
  if (!is.finite(terminal_df) || terminal_df <= 0) stop("Invalid curve.")
  pillars <- purrr::map_chr(helpers, function(h) QuantLib::Date_ISO(h$pillarDate()))
  if (anyDuplicated(pillars)) stop("Duplicate helper pillars, e.g. 12M and 1Y.")
  maturities <- as.Date(purrr::map_chr(helpers, function(h) QuantLib::Date_ISO(h$maturityDate())))
  if ("tenordate" %in% names(q)) {
    supplied <- as.Date(q$tenordate)
    if (any(!is.na(supplied) & supplied != maturities)) {
      stop("Input tenordate differs from helper maturity; check conventions.")
    }
  }
  q$tenordate <- maturities
  repricing <- q |>
    dplyr::mutate(
      pillar_date = as.Date(pillars),
      implied_rate = purrr::map_dbl(helpers, function(h) as.numeric(h$impliedQuote())),
      quote_error_bp = (.data$implied_rate - .data$rate) * 1e4
    )
  if (any(!is.finite(repricing$quote_error_bp)) ||
      max(abs(repricing$quote_error_bp)) > 1e-4) stop("OIS repricing failed.")
  handle <- QuantLib::YieldTermStructureHandle(curve)
  linked <- instance_overnight_index(spec$currency, handle)
  list(
    currency = spec$currency, index_name = spec$index_name,
    marketdate = marketdate, evaluation_date = as.character(marketdate),
    curve = curve, curve_handle = handle, overnight_index = linked, index = linked,
    calendar = calendar, day_counter = day_counter,
    helpers = helpers, helper_vector = helper_vector,
    quotes = q, quote_repricing = repricing, curve_type = curve_type,
    settlement_days = as.integer(settlement_days), payment_lag = as.integer(payment_lag),
    payment_frequency = payment_frequency
  )
}


