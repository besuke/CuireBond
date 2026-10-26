# 属性結合と入力検証。
#
# 結合と既存の検証は、移植した workflows_validate_worksheet()
# （内部で import_offer_sheet()、import_bond_master_attribute_ADP()、import_market_data()、
#   merge_offer_bond_attributes() を使う。R/z0_utility/lagrange/）にそのまま任せる。
# オファーは convert_offer_sheet_Curie() の結果を標準形式のExcelに書き、それを既存関数へ渡す。
# 既存関数は元ファイル名・行番号などCurieの追加列を落とすため、行の順序で戻す
# （既存関数は行数・行順を変えないことを自ら確認している）。
#
# このファイルで追加するのは、Curie独自の問題コードだけである。
#   offer_date_not_valuation_date  オファー日が評価日と異なる（別の日の市場データで補わない）
#   currency_outside_workflow      建て通貨がワークフローの対象外（JPY側はJPY建て、ISO側は円以外）
#   unsupported_product            変動利付・期限前償還・満期一括以外など、現在の計算関数の対象外
#   invalid_ISIN_check_digit       ISINのチェックディジット不一致
#   unsupported_redemption_not_100 償還額が100以外
#   unsupported_redemption_type    満期一括償還（BULLET）以外
#   unsupported_day_count          GaussQuant::day_counter_GQL() が扱えない日数計算方式
#   unsupported_calendar           calendar_from_name_Curie() が扱えないカレンダー
#   offer_currency_mismatch        オファー表の通貨と銘柄属性の通貨が不一致
#   sheet_issue_price_mismatch     オファー表の参考発行価格と銘柄属性の発行価格が不一致
#   missing_JGB_prices             JPY側：評価日のJGB時価がない
#   missing_FX_spot                ISO側：評価日の対円FXスポットがない
#   missing_CCS_basis              ISO側：モデルの契約条件に合う評価日の対円ベーシス気配がない

# 商品・条件が計算関数の対象外であることを示すコード（入力の誤りとは区別して「未対応」とする）
.unsupported_issue_codes_Curie <- c(
  "unsupported_product", "unsupported_coupon_type", "unsupported_coupon_frequency",
  "unsupported_redemption_not_100", "unsupported_redemption_type",
  "unsupported_day_count", "unsupported_calendar"
)

# 既存関数に加えて必須とする属性（債券生成に必要なもの）
.curie_required_attributes <- c(
  "currency", "coupon_type", "coupon_rate", "issue_date", "maturity_date",
  "coupon_frequency", "day_count", "redemption",                      # ここまで既存関数の既定
  "issue_price", "settlement_days", "calendar",
  "payment_convention", "schedule_convention", "maturity_convention", "date_generation"
)

# Curieの追加列（既存関数の結果に、行の順序で戻す）
.curie_offer_metadata <- c(
  "offer_book", "offer_currency", "offer_security_name", "sheet_issue_price",
  "offer_size_unit", "offer_size_multiplier", "offer_source_file", "offer_source_sheet",
  "offer_source_row", "offer_header_row"
)

.supported_Curie <- function(x, f) {
  purrr::map_lgl(x, \(v) !is.na(v) && !inherits(tryCatch(f(v), error = function(e) e), "error"))
}

# workflow      : "JPY"（JPY建て）または "ISO"（円以外の建て通貨）
# offers        : convert_offer_sheet_Curie() の結果（offer_book 列付き）
# standard_file : write_offer_standard_Curie() で書いた標準形式のExcel
# master_path   : 属性取得結果のRDS、ois_paths : import_market_data() が読めるOISファイル
# market        : list(jgb = ...)（JPY）または list(fx = ..., basis = ...)（ISO）
validate_offer_worksheet_Curie <- function(
    workflow = c("JPY", "ISO"), offers, standard_file, master_path, ois_paths, market, valuation_date,
    attribute_policy = c("exact", "previous"),
    curve_instruments = c(JPY = "TONAR_OIS", USD = "SOFR_OIS", EUR = "ESTR_OIS", GBP = "SONIA_OIS", AUD = "AONIA_OIS"),
    required_attributes = .curie_required_attributes) {
  workflow <- match.arg(workflow)
  attribute_policy <- match.arg(attribute_policy)
  out <- workflows_validate_worksheet(
    from = min(offers$offer_date), to = max(offers$offer_date),
    offer_path = list(list(path = standard_file, sheet = "offers")),
    master_path = master_path, market_path = ois_paths,
    attribute_policy = attribute_policy,
    curve_instruments = curve_instruments, required_attributes = required_attributes
  )
  # 既存関数の結果は標準形式ファイルを指すので、元のオファー情報へ戻す
  key <- c("offer_date", "ISIN", "broker_names", "offer_price", "offer_size", "offer_settlementdate")
  if (nrow(out) != nrow(offers) ||
      !isTRUE(all.equal(as.data.frame(out[key]), as.data.frame(offers[key]), check.attributes = FALSE))) {
    stop("既存関数の結果とオファーの行が対応しません。", call. = FALSE)
  }
  out <- out |>
    dplyr::rename(standard_source_file = "offer_source_file", standard_source_sheet = "offer_source_sheet") |>
    dplyr::bind_cols(offers[.curie_offer_metadata])

  flag <- function(x) dplyr::coalesce(as.logical(x), FALSE)
  has_attr <- !is.na(out$attribute_date)
  in_scope <- if (workflow == "JPY") flag(out$currency == "JPY") else flag(out$currency != "JPY")
  checks <- list(
    offer_date_not_valuation_date = flag(out$offer_date != valuation_date),
    currency_outside_workflow = has_attr & !is.na(out$currency) & !in_scope,
    unsupported_product = has_attr & !is.na(out$unsupported_reason),
    invalid_ISIN_check_digit = grepl("^[A-Z]{2}[A-Z0-9]{9}[0-9]$", out$ISIN) & !is_valid_isin_Curie(out$ISIN),
    unsupported_redemption_not_100 = flag(out$redemption > 0 & abs(out$redemption - 100) > 1e-12),
    unsupported_redemption_type = flag(out$redemption_type != "BULLET"),
    unsupported_day_count = has_attr & !is.na(out$day_count) & !.supported_Curie(out$day_count, GaussQuant::day_counter_GQL),
    unsupported_calendar = has_attr & !is.na(out$calendar) & !.supported_Curie(out$calendar, calendar_from_name_Curie),
    offer_currency_mismatch = flag(out$offer_currency != out$currency),
    sheet_issue_price_mismatch = flag(abs(out$sheet_issue_price - out$issue_price) > 1e-9)
  )
  if (workflow == "JPY") {
    n_jgb <- sum(market$jgb$marketdate == valuation_date)
    checks$missing_JGB_prices <- in_scope & n_jgb == 0L
  } else {
    index_names <- sub("_OIS$", "", curve_instruments)
    n_fx <- purrr::map_int(out$currency, function(ccy) {
      sum(market$fx$marketdate == valuation_date & market$fx$base_currency %in% ccy &
            market$fx$quote_currency == "JPY")
    })
    n_basis <- purrr::map_int(out$currency, function(ccy) {
      if (is.na(ccy) || ccy == "JPY" || !ccy %in% names(index_names)) return(0L)
      terms <- basis_terms_for_model_Curie(ccy, index_names[[ccy]])
      market$basis |>
        dplyr::filter(.data$marketdate == valuation_date, .data$currency_pair == paste0(ccy, "JPY")) |>
        dplyr::semi_join(tibble::as_tibble(terms), by = names(terms)) |>
        nrow()
    })
    checks$missing_FX_spot <- in_scope & n_fx == 0L
    checks$missing_CCS_basis <- in_scope & n_basis == 0L
  }
  curie_issues <- purrr::map(seq_len(nrow(out)), \(i) names(checks)[purrr::map_lgl(checks, \(x) x[[i]])])
  out$validation_issues <- purrr::map2(out$validation_issues, curie_issues, c)
  out$validation_status <- dplyr::if_else(lengths(out$validation_issues) == 0L, "valid", "invalid")
  out$validation_message <- purrr::map_chr(out$validation_issues, \(x) paste(x, collapse = "; "))
  # 不合格の理由が「商品・条件が対象外」だけなら、入力の誤りではなく未対応として扱う
  out$unsupported_only <- purrr::map_lgl(out$validation_issues, \(x) {
    length(x) > 0L && all(x %in% .unsupported_issue_codes_Curie)
  })
  out
}
