# 移植元: LagrangeFinance/R/compute_asset_swap_ISO.R（コミット e6614e1。仕様書の基準コミット e955cf4 と同一内容）
# 関数名・引数・計算ロジックは変更していない。この行より下は移植元のまま。
# ------------------------------------------------------------------------
.tasw_ql_GQH <- function(name, ...) {
  ns <- asNamespace("QuantLib")
  if (!exists(name, ns, inherits = FALSE)) {
    stop("Required QuantLib R binding is unavailable: ", name, call. = FALSE)
  }
  get(name, ns, inherits = FALSE)(...)
}

.tasw_gq_GQH <- function(name, ...) {
  get(name, asNamespace("GaussQuant"), inherits = FALSE)(...)
}

.tasw_date_GQH <- function(x) {
  if (is.character(x) || inherits(x, "Date")) {
    return(.tasw_gq_GQH("date_GQL", as.character(x)))
  }
  x
}

.tasw_table_GQH <- function(leg) {
  out <- .tasw_gq_GQH("leg_to_cashflow_tbl_GQL", leg)
  out <- .tasw_gq_GQH("cashflow_standardise_GQL", out)
  if (anyNA(out$payment_date) || any(!is.finite(out$amount))) {
    stop("Invalid cashflow dates or amounts.", call. = FALSE)
  }
  out
}

.tasw_discount_GQH <- function(curve, dates) {
  purrr::map_dbl(dates, function(d) {
    value <- as.numeric(curve$discount(.tasw_date_GQH(as.Date(d, origin = "1970-01-01"))))
    if (length(value) != 1L || !is.finite(value) || value <= 0) {
      stop("Invalid discount factor.", call. = FALSE)
    }
    value
  })
}

compute_asset_swap_ISO <- function(
  bond, clean_price, swap_curve, spread_bp = 0,
  payment_frequency = "Semiannual", payment_lag = 0L,
  index = swap_curve$overnight_index,
  discount_curve = swap_curve$curve,
  spread_day_counter = NULL
) {
  if (is.null(index)) stop("Supply a linked overnight index.")
  .tasw_gq_GQH("set_eval_date_GQL", .tasw_date_GQH(swap_curve$evaluation_date))
  start_ql <- .tasw_ql_GQH("Bond_settlementDate", bond)
  start <- as.Date(.tasw_gq_GQH("safe_iso_GQH", start_ql))
  forecast_handle <- swap_curve$curve_handle
  spread <- spread_bp / 10000
  cf <- .tasw_table_GQH(.tasw_ql_GQH("Bond_cashflows", bond)) |>
    dplyr::filter(payment_date > start)
  if (!nrow(cf)) stop("No remaining bond cashflows.", call. = FALSE)
  maturity <- as.Date(.tasw_gq_GQH("safe_iso_GQH", .tasw_ql_GQH("Bond_maturityDate", bond)))
  nominal <- as.numeric(.tasw_ql_GQH("Bond_notional", bond, start_ql))
  if (abs(tail(cf$amount, 1L) - nominal) > 1e-8 * nominal) {
    stop("Only bullet bonds with redemption at 100 are supported.", call. = FALSE)
  }
  before_dates <- unique(cf$payment_date[cf$payment_date < maturity])
  notionals <- purrr::map_dbl(before_dates, function(d) {
    as.numeric(.tasw_ql_GQH("Bond_notional", bond, .tasw_date_GQH(as.Date(d, origin = "1970-01-01"))))
  })
  if (any(!is.finite(notionals)) || any(abs(notionals - nominal) > 1e-8 * nominal)) {
    stop("Amortising bonds are not supported.", call. = FALSE)
  }
  accrued <- as.numeric(.tasw_ql_GQH("Bond_accruedAmount", bond, start_ql))
  dirty <- clean_price + accrued
  if (is.null(spread_day_counter)) spread_day_counter <- index$dayCounter()
  frequency <- if (is.character(payment_frequency)) {
    .tasw_ql_GQH(paste0("Frequency_", payment_frequency, "_get"))
  } else {
    payment_frequency
  }
  builder <- .tasw_ql_GQH(
    "MakeOIS",
    .tasw_ql_GQH("Period", 1L, "Years"), index, 1
  )
  settings <- list(
    withEffectiveDate = start_ql,
    withTerminationDate = .tasw_date_GQH(maturity),
    withNominal = nominal,
    withPaymentFrequency = frequency,
    withPaymentLag = as.integer(payment_lag),
    withFixedLegDayCount = spread_day_counter
  )
  builder <- purrr::reduce(names(settings), function(b, name) {
    .tasw_ql_GQH(paste0("MakeOIS_", name), b, settings[[name]])
  }, .init = builder)
  ois <- .tasw_ql_GQH("MakeOIS_makeOIS", builder)
  float <- .tasw_table_GQH(ois$overnightLeg())
  unit <- .tasw_table_GQH(ois$fixedLeg())
  if (!identical(float$payment_date, unit$payment_date) || any(unit$amount <= 0)) {
    stop("Fixed/overnight schedules differ; cannot compute spread annuity.", call. = FALSE)
  }
  float <- float |>
    dplyr::mutate(
      discount_factor = .tasw_discount_GQH(discount_curve, payment_date),
      spread_amount = unit$amount
    )
  cf <- cf |>
    dplyr::mutate(
      discount_factor = .tasw_discount_GQH(discount_curve, payment_date),
      present_value = amount * discount_factor
    )
  annuity <- sum(float$spread_amount * float$discount_factor)
  if (!is.finite(annuity) || annuity <= 0) stop("Invalid spread annuity.")

  principal_date <- dplyr::last(float$payment_date)
  principal_df <- dplyr::last(float$discount_factor)
  start_df <- .tasw_discount_GQH(discount_curve, start)
  bond_pv <- sum(cf$present_value)
  float_pv <- sum(float$amount * float$discount_factor)
  principal_pv <- nominal * principal_df
  upfront <- nominal * (dirty / 100 - 1)
  upfront_pv <- upfront * start_df
  npv_zero <- float_pv + principal_pv + upfront_pv - bond_pv
  fair_spread <- -npv_zero / annuity

  signed_cf <- dplyr::bind_rows(
    cf |>
      dplyr::transmute(
        leg = "pay_bond", payment_date, amount = -amount, discount_factor
      ),
    float |>
      dplyr::transmute(
        leg = "receive_overnight", payment_date,
        amount = amount + spread * spread_amount, discount_factor
      ),
    tibble::tibble(
      leg = c("receive_principal", "upfront"),
      payment_date = c(principal_date, start),
      amount = c(nominal, upfront),
      discount_factor = c(principal_df, start_df)
    )
  ) |>
    dplyr::mutate(present_value = amount * discount_factor)
  structure(list(
    cashflows = signed_cf, bond = bond, ois = ois, index = index,
    forecast_handle = forecast_handle, discount_curve = discount_curve,
    valuation = tibble::tibble(
      clean_price = clean_price, accrued_per_100 = accrued, dirty_price = dirty,
      nominal = nominal, spread = spread, fair_spread = fair_spread,
      fair_spread_bp = fair_spread * 1e4, spread_annuity = annuity,
      npv = npv_zero + spread * annuity,
      bond_leg_pv = -bond_pv, overnight_leg_pv = float_pv + spread * annuity,
      principal_pv = principal_pv, upfront_pv = upfront_pv
    ),
    price_sensitivity = nominal * start_df / 100,
    effective_date = start, maturity_date = maturity,
    payment_frequency = payment_frequency, payment_lag = payment_lag
  ), class = "asset_swap_valuation")
}

asset_swap_fair_price <- function(asset_swap) {
  v <- asset_swap$valuation
  v$clean_price - v$npv / asset_swap$price_sensitivity
}

asset_swap_price_table <- function(asset_swap, prices) {
  if (!is.numeric(prices) || any(!is.finite(prices)) || any(prices <= 0)) {
    stop("prices must contain positive finite numbers.")
  }
  v <- asset_swap$valuation
  tibble::tibble(clean_price = prices) |>
    dplyr::mutate(
      AS_spread_pct = 100 * (
        v$fair_spread -
          (clean_price - v$clean_price) *
            asset_swap$price_sensitivity / v$spread_annuity
      ),
      AS_spread_bp = AS_spread_pct * 100
    ) |>
    dplyr::select(clean_price, AS_spread_bp, AS_spread_pct)
}

