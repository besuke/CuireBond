# 移植元: LagrangeFinance/R/workflows_validate_worksheet.R（コミット e6614e1。仕様書の基準コミット e955cf4 と同一内容）
# 関数名・引数・計算ロジックは変更していない。この行より下は移植元のまま。
# ------------------------------------------------------------------------
# Source R/import_data.R first.
# ========================================================================
# merge_offer_bond_attributes.R
# ========================================================================
# Replace stale attributes, retaining all offer rows and their original order.
merge_offer_bond_attributes <- function(offers, attributes, attribute_policy = c("exact", "previous")) {
  .ws_require(c("collapse", "dplyr", "purrr", "tibble"))
  attribute_policy <- match.arg(attribute_policy)
  .ws_columns(offers, .ws_offer_columns); .ws_columns(attributes, c("ISIN", "attribute_date"))
  offers$ISIN <- .ws_isin(offers$ISIN)
  offers$offer_date <- .ws_date(offers$offer_date, "offer_date")
  attributes$ISIN <- .ws_isin(attributes$ISIN)
  attributes$attribute_date <- .ws_date(attributes$attribute_date, "attribute_date")
  if (anyNA(offers$offer_date) || anyNA(attributes$attribute_date) || any(.ws_blank(attributes$ISIN)))
    stop("Missing date or master ISIN.", call. = FALSE)
  if (anyDuplicated(attributes[c("ISIN", "attribute_date")])) stop("Duplicate master keys.", call. = FALSE)
  base <- offers[, c(.ws_offer_columns, intersect(c("offer_source_file", "offer_source_sheet"), names(offers))), drop = FALSE]
  base$.worksheet_row <- seq_len(nrow(base))
  attrs <- attributes[, !grepl("^offer_", names(attributes)) & names(attributes) != "broker_names", drop = FALSE]
  reserved <- c(".worksheet_row", "market_quotes", "jpy_market_quotes", "validation_status", "validation_message", "validation_issues", "marketdate", "attribute_age_days")
  if (any(reserved %in% names(attrs))) stop("Master contains reserved worksheet column names.", call. = FALSE)
  left_join <- function(x, y) {
    # Uniqueness is checked explicitly so older collapse versions are supported.
    if (anyDuplicated(y$ISIN)) stop("Master selection must have one row per ISIN.", call. = FALSE)
    collapse::join(x, y, on = "ISIN", how = "left", multiple = FALSE, sort = FALSE) |> tibble::as_tibble()
  }
  out <- if (!nrow(base)) left_join(base, attrs[0, ]) else {
    purrr::map_dfr(unique(base$offer_date), function(day) {
      selected <- if (attribute_policy == "exact") dplyr::filter(attrs, attribute_date == day) else {
        attrs |> dplyr::filter(attribute_date <= day) |> dplyr::group_by(ISIN) |>
          dplyr::slice_max(attribute_date, n = 1L, with_ties = FALSE) |> dplyr::ungroup()
      }
      left_join(dplyr::filter(base, offer_date == day), selected)
    })
  }
  if (nrow(out) != nrow(base)) stop("Join changed offer row count.", call. = FALSE)
  out |> dplyr::arrange(.worksheet_row) |> dplyr::select(-.worksheet_row)
}

# ========================================================================
# workflows_validate_worksheet.R
# ========================================================================
# Prepare a historical worksheet, not ASW pricing. Missing inputs remain visible.
workflows_validate_worksheet <- function(from, to, offer_path, master_path, market_path,
    attribute_policy = c("exact", "previous"), master_column_map = NULL,
    master_snapshot_date = NULL, market_column_map = NULL, market_encoding = "UTF-8",
    curve_instruments = c(JPY = "TONAR_OIS", USD = "SOFR_OIS", EUR = "ESTR_OIS", GBP = "SONIA_OIS", AUD = "AONIA_OIS"),
    required_attributes = c("currency", "coupon_type", "coupon_rate", "issue_date", "maturity_date", "coupon_frequency", "day_count", "redemption")) {
  .ws_require(c("collapse", "dplyr", "purrr", "tibble", "readxl", "readr"))
  period <- .ws_range(from, to); attribute_policy <- match.arg(attribute_policy)
  if (is.null(names(curve_instruments)) || anyNA(curve_instruments) || anyNA(names(curve_instruments)) ||
      any(!nzchar(names(curve_instruments))) || anyDuplicated(names(curve_instruments)))
    stop("curve_instruments must be uniquely named by currency.", call. = FALSE)
  required_attributes <- unique(required_attributes)
  batches <- if (is.character(offer_path)) purrr::map(offer_path, function(p) list(path = p)) else offer_path
  if (!length(batches)) stop("No offer batches supplied.", call. = FALSE)
  offers <- purrr::map_dfr(batches, function(batch) {
    if (!is.list(batch) || is.null(batch$path) || any(c("from", "to") %in% names(batch)))
      stop("Each batch needs path; from/to belong to the workflow.", call. = FALSE)
    do.call(import_offer_sheet, c(batch, list(from = period$from, to = period$to)))
  })
  attributes <- import_bond_master_attribute_ADP(master_path, ISIN = unique(offers$ISIN),
    from = if (attribute_policy == "exact") period$from else NULL, to = period$to,
    column_map = master_column_map, snapshot_date = master_snapshot_date)
  market <- import_market_data(market_path, period$from, period$to, market_column_map, market_encoding)
  out <- merge_offer_bond_attributes(offers, attributes, attribute_policy)
  fields <- unique(c(required_attributes, "currency", "coupon_type", "coupon_rate", "issue_date", "maturity_date", "coupon_frequency", "day_count", "redemption"))
  absent <- setdiff(fields, names(out))
  out[absent] <- rep(list(rep(NA, nrow(out))), length(absent))
  out <- out |> dplyr::mutate(
    currency = toupper(trimws(as.character(currency))),
    coupon_type = toupper(trimws(as.character(coupon_type))),
    coupon_rate = .ws_number(coupon_rate, "coupon_rate (decimal)"),
    coupon_frequency = .ws_number(coupon_frequency, "coupon_frequency (payments/year)"),
    redemption = .ws_number(redemption, "redemption (per 100)"),
    issue_date = .ws_date(issue_date, "issue_date"), maturity_date = .ws_date(maturity_date, "maturity_date"))
  keys <- out[c("offer_date", "ISIN", "broker_names")]
  duplicate <- duplicated(keys) | duplicated(keys, fromLast = TRUE)
  quote_set <- function(day, ccy) {
    if (is.na(ccy) || !ccy %in% names(curve_instruments)) return(market[0, ])
    id <- unname(curve_instruments[ccy])
    dplyr::filter(market, marketdate == day, currency == ccy, instrument == id)
  }
  out$market_quotes <- purrr::map2(out$offer_date, out$currency, quote_set)
  out$jpy_market_quotes <- purrr::map(out$offer_date, function(day) quote_set(day, "JPY"))
  out$marketdate <- out$offer_date
  out$attribute_age_days <- as.integer(out$offer_date - out$attribute_date)
  out$validation_issues <- purrr::map(seq_len(nrow(out)), function(i) {
    row <- out[i, ]
    missing <- required_attributes[purrr::map_lgl(row[required_attributes], function(x) .ws_blank(x)[1])]
    c(
      if (.ws_blank(row$ISIN)) "missing_ISIN"
      else if (!grepl("^[A-Z]{2}[A-Z0-9]{9}[0-9]$", row$ISIN)) "invalid_ISIN_format",
      if (.ws_blank(row$broker_names)) "missing_broker_names",
      if (duplicate[i]) "duplicate_offer_key",
      if (is.na(row$offer_price) || row$offer_price <= 0) "invalid_offer_price",
      if (is.na(row$offer_size) || row$offer_size <= 0) "invalid_offer_size",
      if (is.na(row$offer_settlementdate)) "missing_offer_settlementdate"
      else if (row$offer_settlementdate < row$offer_date) "settlement_before_offer",
      if (is.na(row$attribute_date)) "missing_bond_attributes",
      if (length(missing)) paste0("missing_attribute:", missing),
      if (!.ws_blank(row$currency) && !row$currency %in% names(curve_instruments)) "unsupported_currency",
      if (!.ws_blank(row$coupon_type) && row$coupon_type != "FIXED") "unsupported_coupon_type",
      if (!is.na(row$coupon_rate) && row$coupon_rate < 0) "invalid_coupon_rate",
      if (!is.na(row$coupon_frequency) && !row$coupon_frequency %in% c(1, 2, 4, 12)) "unsupported_coupon_frequency",
      if (!is.na(row$redemption) && row$redemption <= 0) "invalid_redemption",
      if (!is.na(row$issue_date) && !is.na(row$maturity_date) && row$issue_date >= row$maturity_date) "invalid_bond_dates",
      if (!is.na(row$issue_date) && !is.na(row$offer_settlementdate) && row$issue_date > row$offer_settlementdate) "issue_after_settlement",
      if (!is.na(row$maturity_date) && !is.na(row$offer_settlementdate) && row$maturity_date <= row$offer_settlementdate) "matured_at_settlement",
      if (!nrow(row$market_quotes[[1]])) "missing_currency_market_data",
      if (!nrow(row$jpy_market_quotes[[1]])) "missing_JPY_market_data"
    )
  })
  out$validation_status <- ifelse(lengths(out$validation_issues) == 0L, "valid", "invalid")
  out$validation_message <- purrr::map_chr(out$validation_issues, paste, collapse = "; ")
  attr(out, "validation_scope") <- "Input checks only; curve calibration/coverage, pricing conventions, optionality, FX and CCS basis are not checked."
  out
}
