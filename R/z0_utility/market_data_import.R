# 市場データの読込と前処理（FXスポット、通貨ベーシス、JGB時価→複利利回り）。
# OIS気配は移植した import_market_data()（R/z0_utility/lagrange/import_data.R）で読む（R/z0_utility/import_settings.R）。
# どのファイルも取込設定（text_import_settings_Curie）で区切り文字・文字コード・列名を指定できる。
# 取得時刻の列 quote_time は任意。あれば出力へ残す。

.ensure_quote_time_Curie <- function(x) {
  if (!"quote_time" %in% names(x)) x$quote_time <- NA_character_
  x
}

.require_columns_Curie <- function(x, columns, label) {
  missing <- setdiff(columns, names(x))
  if (length(missing)) stop(label, "に必須列がありません: ", paste(missing, collapse = ", "), call. = FALSE)
  x
}

# ---- FXスポット ----------------------------------------------------------
# fx_spot は base_currency 1単位当たりの quote_currency（円）
import_fx_spot_Curie <- function(path, settings = text_import_settings_Curie()) {
  read_text_table_Curie(path, settings) |>
    .require_columns_Curie(c("marketdate", "currency_pair", "base_currency", "quote_currency",
                             "fx_spot", "spot_date"), "FXスポット") |>
    .ensure_quote_time_Curie() |>
    dplyr::mutate(
      marketdate = parse_date_Curie(.data$marketdate, "marketdate"),
      spot_date = parse_date_Curie(.data$spot_date, "spot_date"),
      fx_spot = parse_number_Curie(.data$fx_spot, "fx_spot"),
      dplyr::across(c("currency_pair", "base_currency", "quote_currency"), stringr::str_to_upper),
      fx_quote_id = paste("FX", .data$source_file, .data$marketdate, .data$currency_pair, sep = "|")
    )
}

# ---- 通貨ベーシス --------------------------------------------------------
.basis_term_columns_Curie <- c(
  "spread_leg", "pay_leg", "receive_leg", "notional_type", "collateral_currency"
)

import_ccs_basis_Curie <- function(path, settings = text_import_settings_Curie()) {
  read_text_table_Curie(path, settings) |>
    .require_columns_Curie(c("marketdate", "currency_pair", "currency", "maturity_date", "basis_bp",
                             "quote_source", .basis_term_columns_Curie), "通貨ベーシス") |>
    .ensure_quote_time_Curie() |>
    dplyr::mutate(
      marketdate = parse_date_Curie(.data$marketdate, "marketdate"),
      maturity_date = parse_date_Curie(.data$maturity_date, "maturity_date"),
      basis_bp = parse_number_Curie(.data$basis_bp, "basis_bp"),
      dplyr::across(c("currency_pair", "currency"), stringr::str_to_upper)
    )
}

# ---- JGB：時価ファイル（単価と必要な銘柄属性）から複利利回り ---------------
# 単価は額面100当たりのクリーン価格。price_type が clean 以外の行は計算しない。
# 利回り計算に必要な属性（クーポン・利払回数・償還日・償還額）も同じファイルから読む。
.jgb_columns_Curie <- c(
  "marketdate", "ISIN", "clean_price", "price_type", "settlement_date",
  "coupon_rate", "coupon_frequency", "maturity_date", "redemption"
)

import_jgb_file_Curie <- function(path, settings = text_import_settings_Curie()) {
  read_text_table_Curie(path, settings) |>
    .require_columns_Curie(.jgb_columns_Curie, "JGB時価ファイル") |>
    .ensure_quote_time_Curie() |>
    dplyr::mutate(
      marketdate = parse_date_Curie(.data$marketdate, "marketdate"),
      settlement_date = parse_date_Curie(.data$settlement_date, "settlement_date"),
      maturity_date = parse_date_Curie(.data$maturity_date, "maturity_date"),
      clean_price = parse_number_Curie(.data$clean_price, "clean_price"),
      coupon_rate = parse_number_Curie(.data$coupon_rate, "coupon_rate"),
      coupon_frequency = parse_number_Curie(.data$coupon_frequency, "coupon_frequency"),
      redemption = parse_number_Curie(.data$redemption, "redemption"),
      ISIN = stringr::str_to_upper(stringr::str_trim(.data$ISIN)),
      price_type = stringr::str_to_lower(.data$price_type),
      jgb_price_id = paste("JGB_PRICE", .data$source_file, .data$marketdate, .data$ISIN, sep = "|")
    )
}

