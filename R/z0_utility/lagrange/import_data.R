# 移植元: LagrangeFinance/R/import_data.R（コミット e6614e1。仕様書の基準コミット e955cf4 と同一内容）
# 関数名・引数・計算ロジックは変更していない。この行より下は移植元のまま。
# ------------------------------------------------------------------------
# Import functions and their private helpers. No imports run when sourced.
# ========================================================================
# worksheet_helpers.R
# ========================================================================
# Internal helpers shared by the worksheet import functions.
.ws_require <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Install packages: ", paste(missing, collapse = ", "), call. = FALSE)
}
.ws_columns <- function(x, required) {
  missing <- setdiff(required, names(x))
  if (length(missing)) stop("Missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  invisible(x)
}
.ws_names <- function(x, column_map = NULL) {
  if (!is.null(column_map)) {
    if (is.null(names(column_map)) || anyNA(column_map) || anyNA(names(column_map)) ||
        any(!nzchar(names(column_map))) || anyDuplicated(names(column_map)) || anyDuplicated(unname(column_map)))
      stop("column_map must be c(canonical_name = 'source name') with unique names.", call. = FALSE)
    .ws_columns(x, unname(column_map))
    names(x)[match(unname(column_map), names(x))] <- names(column_map)
  }
  if (anyDuplicated(names(x))) stop("Duplicate column names after mapping.", call. = FALSE)
  tibble::as_tibble(x)
}
.ws_files <- function(path, pattern) {
  if (!length(path) || anyNA(path)) stop("Specify input paths.", call. = FALSE)
  files <- purrr::map(path, function(p) {
    if (dir.exists(p)) list.files(p, pattern = pattern, full.names = TRUE, ignore.case = TRUE)
    else if (file.exists(p) && grepl(pattern, p, ignore.case = TRUE)) p
    else stop("Missing file or unsupported extension: ", p, call. = FALSE)
  }) |> unlist(use.names = FALSE) |> unique() |> sort()
  files <- files[!grepl("^~\\$", basename(files))]
  if (!length(files)) stop("No matching input files.", call. = FALSE)
  files
}
.ws_blank <- function(x) is.na(x) | trimws(as.character(x)) == ""
.ws_isin <- function(x) toupper(trimws(as.character(x)))
.ws_date <- function(x, label) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x, tz = "UTC"))
  text <- gsub("/", "-", trimws(as.character(x)), fixed = TRUE)
  blank <- .ws_blank(text)
  serial <- !blank & grepl("^[0-9]+([.][0-9]+)?$", text)
  out <- rep(as.Date(NA), length(text))
  out[serial] <- as.Date(as.numeric(text[serial]), origin = "1899-12-30")
  idx <- !blank & !serial
  valid <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", text[idx])
  if (any(!valid)) stop("Use YYYY-MM-DD dates in ", label, call. = FALSE)
  out[idx] <- suppressWarnings(as.Date(text[idx], format = "%Y-%m-%d"))
  if (any(!blank & is.na(out))) stop("Invalid date in ", label, call. = FALSE)
  out
}
.ws_range <- function(from = NULL, to = NULL) {
  a <- if (is.null(from)) as.Date("1900-01-01") else .ws_date(from, "from")
  b <- if (is.null(to)) as.Date("9999-12-31") else .ws_date(to, "to")
  if (length(a) != 1L || length(b) != 1L || is.na(a) || is.na(b) || a > b)
    stop("from/to must be single dates with from <= to.", call. = FALSE)
  list(from = a, to = b)
}
.ws_number <- function(x, label) {
  text <- trimws(as.character(x)); blank <- .ws_blank(text)
  valid <- blank | grepl("^[+-]?([0-9]+|[0-9]{1,3}(,[0-9]{3})+)([.][0-9]+)?([eE][+-]?[0-9]+)?$", text)
  out <- suppressWarnings(as.numeric(gsub(",", "", text, fixed = TRUE)))
  out[blank] <- NA_real_
  if (any(!blank & (!valid | !is.finite(out)))) stop("Invalid number in ", label, call. = FALSE)
  out
}
.ws_offer_columns <- c("offer_date", "ISIN", "broker_names", "offer_price", "offer_size", "offer_settlementdate")

# ========================================================================
# import_offer_sheet.R
# ========================================================================
# Excel import. Each call may target one sheet across one or several workbooks.
import_offer_sheet <- function(path, sheet = 1, from = NULL, to = NULL,
                               column_map = NULL, skip = 0L) {
  .ws_require(c("readxl", "dplyr", "purrr", "tibble"))
  period <- .ws_range(from, to)
  purrr::map_dfr(.ws_files(path, "\\.xlsx?$"), function(file) {
    x <- readxl::read_excel(file, sheet = sheet, skip = skip, col_types = "text",
                            .name_repair = "check_unique") |> .ws_names(column_map)
    .ws_columns(x, .ws_offer_columns)
    blank_row <- purrr::pmap_lgl(x[.ws_offer_columns], function(...) all(.ws_blank(c(...))))
    x <- x[!blank_row, ]
    x <- x |> dplyr::mutate(
      offer_date = .ws_date(offer_date, "offer_date"), ISIN = .ws_isin(ISIN),
      broker_names = trimws(broker_names), offer_price = .ws_number(offer_price, "offer_price"),
      offer_size = .ws_number(offer_size, "offer_size"),
      offer_settlementdate = .ws_date(offer_settlementdate, "offer_settlementdate"))
    if (anyNA(x$offer_date)) stop("Missing offer_date: cannot select historical period.", call. = FALSE)
    x |> dplyr::filter(offer_date >= period$from, offer_date <= period$to) |>
      dplyr::mutate(offer_source_file = normalizePath(file, winslash = "/"),
                    offer_source_sheet = as.character(sheet))
  })
}

# ========================================================================
# import_bond_master_attribute_ADP.R
# ========================================================================
# Reads an Aladdin-exported data.frame RDS. No live API call.
import_bond_master_attribute_ADP <- function(path, ISIN = NULL, from = NULL, to = NULL,
                                            column_map = NULL, snapshot_date = NULL) {
  .ws_require(c("dplyr", "purrr", "tibble"))
  period <- .ws_range(from, to); files <- .ws_files(path, "\\.rds$")
  if (!is.null(snapshot_date) && length(files) != 1L)
    stop("snapshot_date can label only one undated RDS.", call. = FALSE)
  x <- purrr::map_dfr(files, function(file) {
    x <- readRDS(file)
    if (!is.data.frame(x)) stop("RDS must contain a data.frame/tibble: ", file, call. = FALSE)
    x <- .ws_names(x, column_map); .ws_columns(x, "ISIN")
    if (!"attribute_date" %in% names(x)) {
      if (is.null(snapshot_date)) stop("RDS needs attribute_date or explicit snapshot_date.", call. = FALSE)
      date <- .ws_date(snapshot_date, "snapshot_date")
      if (length(date) != 1L || is.na(date)) stop("snapshot_date must be one date.", call. = FALSE)
      x$attribute_date <- rep(date, nrow(x))
    } else if (!is.null(snapshot_date)) stop("RDS already has attribute_date; omit snapshot_date.", call. = FALSE)
    x$ISIN <- .ws_isin(x$ISIN)
    x$attribute_date <- .ws_date(x$attribute_date, "attribute_date")
    if (any(.ws_blank(x$ISIN)) || anyNA(x$attribute_date)) stop("Missing master key.", call. = FALSE)
    # Never allow a master export to replace any offer terms.
    x <- x[, !grepl("^offer_", names(x)) & names(x) != "broker_names", drop = FALSE]
    x$attribute_source_file <- normalizePath(file, winslash = "/")
    x
  }) |> dplyr::filter(attribute_date >= period$from, attribute_date <= period$to)
  if (!is.null(ISIN)) x <- dplyr::filter(x, .data$ISIN %in% .ws_isin(ISIN))
  if (anyDuplicated(x[c("ISIN", "attribute_date")])) stop("Duplicate ISIN + attribute_date in master.", call. = FALSE)
  x
}

# ========================================================================
# import_market_data.R
# ========================================================================
# Pipe-delimited text, with explicit units: rate decimal OR rate_pct percent.
import_market_data <- function(path, from = NULL, to = NULL, column_map = NULL, encoding = "UTF-8") {
  .ws_require(c("readr", "dplyr", "purrr", "tibble"))
  period <- .ws_range(from, to)
  x <- purrr::map_dfr(.ws_files(path, "\\.(txt|psv|csv)$"), function(file) {
    x <- readr::read_delim(file, delim = "|", col_types = readr::cols(.default = "c"),
      locale = readr::locale(encoding = encoding), trim_ws = TRUE,
      name_repair = "check_unique", show_col_types = FALSE, progress = FALSE)
    if (nrow(readr::problems(x))) stop("Malformed pipe-delimited input: ", file, call. = FALSE)
    x <- .ws_names(x, column_map)
    .ws_columns(x, c("marketdate", "currency", "instrument", "tenor"))
    if (!any(c("rate", "rate_pct") %in% names(x))) stop("Market needs rate or rate_pct.", call. = FALSE)
    x$marketdate <- .ws_date(x$marketdate, "marketdate")
    x$currency <- toupper(trimws(x$currency)); x$instrument <- trimws(x$instrument)
    x$tenor <- toupper(trimws(x$tenor))
    if ("rate" %in% names(x)) x$rate <- .ws_number(x$rate, "rate")
    if ("rate_pct" %in% names(x)) x$rate_pct <- .ws_number(x$rate_pct, "rate_pct")
    if (!"rate" %in% names(x)) x$rate <- x$rate_pct / 100
    if (!"rate_pct" %in% names(x)) x$rate_pct <- x$rate * 100
    if (anyNA(x$rate) || anyNA(x$rate_pct) || any(abs(x$rate * 100 - x$rate_pct) > 1e-8))
      stop("Missing or inconsistent market rates.", call. = FALSE)
    if (anyNA(x$marketdate) || any(.ws_blank(x$currency)) || any(.ws_blank(x$instrument)) || any(.ws_blank(x$tenor)))
      stop("Missing market key.", call. = FALSE)
    x$tenordate <- if ("tenordate" %in% names(x)) .ws_date(x$tenordate, "tenordate") else rep(as.Date(NA), nrow(x))
    x$market_source_file <- normalizePath(file, winslash = "/")
    x
  }) |> dplyr::filter(marketdate >= period$from, marketdate <= period$to)
  if (anyDuplicated(x[c("marketdate", "instrument", "tenor")])) stop("Duplicate marketdate + instrument + tenor.", call. = FALSE)
  x
}

# ========================================================================
# import_ois_curves.R
# ========================================================================


import_ois_curves <- function(
    path,
    currency,
    marketdate = NULL,
    quote = c("mid", "bid", "ask")
) {
  quote <- match.arg(quote)
  currency <- match.arg(
    toupper(currency),
    c("JPY", "USD", "EUR", "GBP", "AUD")
  )
  
  # Excel日付番号、YYYY-MM-DD、YYYY/MM/DD、ISO日時に対応
  parse_marketdate <- function(x) {
    x <- trimws(as.character(x))
    result <- rep(as.Date(NA), length(x))
    
    is_serial <- !is.na(x) &
      grepl("^[0-9]+([.][0-9]+)?$", x)
    
    result[is_serial] <- as.Date(
      as.numeric(x[is_serial]),
      origin = "1899-12-30"
    )
    
    is_text <- !is.na(x) & nzchar(x) & !is_serial
    
    result[is_text] <- as.Date(
      gsub("/", "-", substr(x[is_text], 1, 10)),
      format = "%Y-%m-%d"
    )
    
    result
  }
  
  tbl <- readxl::read_excel(
    path = path,
    sheet = currency,
    range = readxl::cell_cols("A:F"),
    col_types = c(
      "text", "text", "text",
      "numeric", "numeric", "numeric"
    )
  ) |>
    tibble::as_tibble() |>
    dplyr::rename(
      ask = `Ask Yield`,
      mid = `Mid Yield`,
      bid = `Bid Yield`
    ) |>
    dplyr::mutate(
      marketdate = parse_marketdate(.data$marketdate),
      currency = .env$currency,
      rate_pct = .data[[quote]],
      rate = .data$rate_pct / 100,
      instrument = dplyr::case_when(
        .data$ticker == "SONIO/N Index" ~ "SONIA_FIXING",
        .env$currency == "AUD" & !grepl("^ADSO", .data$ticker) ~ "AUD_SWAP_UNCONFIRMED",
        TRUE ~ paste0(c(JPY = "TONAR", USD = "SOFR", EUR = "ESTR",
                       GBP = "SONIA", AUD = "AONIA")[[.env$currency]], "_OIS")
      ),
      instrument_type = dplyr::case_when(
        .data$instrument == "SONIA_FIXING" ~ "overnight_fixing",
        .data$instrument == "AUD_SWAP_UNCONFIRMED" ~ "unconfirmed_swap",
        TRUE ~ "spot_ois"
      ),
      tenordate = as.Date(NA),
      source = "Bloomberg Excel (image transcription)"
    ) |>
    dplyr::select(
      marketdate, currency, tenor, ticker,
      instrument, instrument_type, tenordate,
      ask, mid, bid, rate_pct, rate, source
    )
  
  if (anyNA(tbl[c("marketdate", "tenor", "ticker")]) ||
      any(!is.finite(tbl$rate_pct))) {
    stop("Missing values in date, Tenor, Ticker or the selected quote.")
  }
  
  if (!is.null(marketdate)) {
    selected_date <- parse_marketdate(marketdate)
    
    if (length(selected_date) != 1L || is.na(selected_date)) {
      stop("Specify one valid date for marketdate.")
    }
    
    tbl <- tbl |>
      dplyr::filter(.data$marketdate == .env$selected_date)
  }
  
  if (nrow(tbl) == 0L) {
    stop("No market data matches the conditions.")
  }
  
  if (anyDuplicated(tbl[c("marketdate", "ticker", "tenor")])) {
    stop("Duplicated marketdate, Ticker and Tenor.")
  }
  
  tbl
}
