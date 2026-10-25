# 市場データの読込と前処理（FXスポット、通貨ベーシス、JGB時価→複利利回り）。
# OIS気配は移植した import_market_data()（R/lagrange/import_data.R）で読む（R/import_settings.R）。
# どのファイルも取込設定（text_import_settings_CURIE）で区切り文字・文字コード・列名を指定できる。
# 取得時刻の列 quote_time は任意。あれば出力へ残す。

.ensure_quote_time_CURIE <- function(x) {
  if (!"quote_time" %in% names(x)) x$quote_time <- NA_character_
  x
}

.require_columns_CURIE <- function(x, columns, label) {
  missing <- setdiff(columns, names(x))
  if (length(missing)) stop(label, "に必須列がありません: ", paste(missing, collapse = ", "), call. = FALSE)
  x
}

# ---- FXスポット ----------------------------------------------------------
# fx_spot は base_currency 1単位当たりの quote_currency（円）
import_fx_spot_CURIE <- function(path, settings = text_import_settings_CURIE()) {
  read_text_table_CURIE(path, settings) |>
    .require_columns_CURIE(c("marketdate", "currency_pair", "base_currency", "quote_currency",
                             "fx_spot", "spot_date"), "FXスポット") |>
    .ensure_quote_time_CURIE() |>
    dplyr::mutate(
      marketdate = parse_date_CURIE(.data$marketdate, "marketdate"),
      spot_date = parse_date_CURIE(.data$spot_date, "spot_date"),
      fx_spot = parse_number_CURIE(.data$fx_spot, "fx_spot"),
      dplyr::across(c("currency_pair", "base_currency", "quote_currency"), stringr::str_to_upper),
      fx_quote_id = paste("FX", .data$source_file, .data$marketdate, .data$currency_pair, sep = "|")
    )
}

# ---- 通貨ベーシス --------------------------------------------------------
.basis_term_columns_CURIE <- c(
  "spread_leg", "pay_leg", "receive_leg", "notional_type", "collateral_currency"
)

import_ccs_basis_CURIE <- function(path, settings = text_import_settings_CURIE()) {
  read_text_table_CURIE(path, settings) |>
    .require_columns_CURIE(c("marketdate", "currency_pair", "currency", "maturity_date", "basis_bp",
                             "quote_source", .basis_term_columns_CURIE), "通貨ベーシス") |>
    .ensure_quote_time_CURIE() |>
    dplyr::mutate(
      marketdate = parse_date_CURIE(.data$marketdate, "marketdate"),
      maturity_date = parse_date_CURIE(.data$maturity_date, "maturity_date"),
      basis_bp = parse_number_CURIE(.data$basis_bp, "basis_bp"),
      dplyr::across(c("currency_pair", "currency"), stringr::str_to_upper)
    )
}

# 同一の通貨ペア・市場日・契約条件・償還日の気配を算術平均する
average_ccs_basis_CURIE <- function(basis) {
  basis |>
    dplyr::summarise(
      basis_bp_mean = mean(.data$basis_bp),
      n_quotes = dplyr::n(),
      quote_sources = paste(sort(unique(.data$quote_source)), collapse = ", "),
      quote_times = paste(sort(unique(stats::na.omit(.data$quote_time))), collapse = ", "),
      source_file = paste(sort(unique(.data$source_file)), collapse = ", "),
      .by = dplyr::all_of(c("marketdate", "currency_pair", "currency", .basis_term_columns_CURIE, "maturity_date"))
    ) |>
    dplyr::arrange(.data$currency_pair, .data$marketdate, .data$maturity_date)
}

# 現行の円換算ASW（compute_asset_swap_basis_JPY）が前提とする契約条件
basis_terms_for_model_CURIE <- function(currency, index_name) {
  list(
    spread_leg = "JPY",
    pay_leg = paste0(currency, " ", index_name, " flat"),
    receive_leg = "JPY TONAR + basis_bp",
    notional_type = "constant",
    collateral_currency = "JPY"
  )
}

# 償還日ごとの観測値を、起点日から償還日までの残存年数（日数/365）を横軸として
# 自然3次スプラインで補間する（通貨ベーシスとJGB利回りで共通）。
# 横軸を一定倍・平行移動しても自然3次スプラインの補間値は変わらないため、
# 年日数（365）や起点日の取り方は、すべての点で同じであれば結果に影響しない。
# 観測範囲外は外挿せず、value = NA と理由を返す。
spline_by_maturity_CURIE <- function(maturity_dates, values, origin_date, target_maturity, label) {
  if (anyDuplicated(maturity_dates)) stop(label, "：同じ償還日の点は平均してから渡してください。", call. = FALSE)
  if (length(maturity_dates) < 2L) {
    return(list(value = NA_real_, message = paste0(label, "の償還日が2点未満のため補間できない")))
  }
  x <- as.numeric(maturity_dates - origin_date) / 365
  x0 <- as.numeric(target_maturity - origin_date) / 365
  if (x0 < min(x) || x0 > max(x)) {
    return(list(value = NA_real_, message = paste0(
      "償還日 ", target_maturity, " が", label, "の範囲（", min(maturity_dates), "〜",
      max(maturity_dates), "）外。外挿しない")))
  }
  list(value = stats::splinefun(x, values, method = "natural")(x0), message = "")
}

# 平均ベーシスを、評価日から償還日までの残存年数を横軸として自然3次スプラインで補間する。
# CCSカーブのブートストラップではなく、ベーシス気配そのものの補間である。
interpolate_ccs_basis_CURIE <- function(basis_avg, valuation_date, maturity_date, currency, terms) {
  pts <- basis_avg |>
    dplyr::filter(.data$marketdate == valuation_date, .data$currency_pair == paste0(currency, "JPY")) |>
    dplyr::semi_join(tibble::as_tibble(terms), by = names(terms)) |>
    dplyr::arrange(.data$maturity_date)
  fit <- spline_by_maturity_CURIE(pts$maturity_date, pts$basis_bp_mean, valuation_date,
                                  maturity_date, "ベーシス気配")
  tibble::tibble(
    basis_bp = fit$value, basis_n_points = nrow(pts),
    basis_marketdate = if (nrow(pts)) valuation_date else as.Date(NA),
    basis_quote_time = paste(unique(pts$quote_times[nzchar(pts$quote_times)]), collapse = ", "),
    basis_min_maturity = if (nrow(pts)) min(pts$maturity_date) else as.Date(NA),
    basis_max_maturity = if (nrow(pts)) max(pts$maturity_date) else as.Date(NA),
    basis_quote_set_id = paste("CCS_BASIS", paste(unique(pts$source_file), collapse = ","), valuation_date,
                               paste0(currency, "JPY"), terms$notional_type, sep = "|"),
    basis_method = paste("同一の通貨ペア・市場日・契約条件・償還日の気配を算術平均し、",
                         "評価日からの残存年数で自然3次スプライン補間（外挿なし）。",
                         "ベーシス気配の補間であり、CCSカーブのブートストラップではない"),
    basis_message = fit$message
  )
}

# ---- JGB：時価ファイル（単価と必要な銘柄属性）から複利利回り ---------------
# 単価は額面100当たりのクリーン価格。price_type が clean 以外の行は計算しない。
# 利回り計算に必要な属性（クーポン・利払回数・償還日・償還額）も同じファイルから読む。
.jgb_columns_CURIE <- c(
  "marketdate", "ISIN", "clean_price", "price_type", "settlement_date",
  "coupon_rate", "coupon_frequency", "maturity_date", "redemption"
)

import_jgb_file_CURIE <- function(path, settings = text_import_settings_CURIE()) {
  read_text_table_CURIE(path, settings) |>
    .require_columns_CURIE(.jgb_columns_CURIE, "JGB時価ファイル") |>
    .ensure_quote_time_CURIE() |>
    dplyr::mutate(
      marketdate = parse_date_CURIE(.data$marketdate, "marketdate"),
      settlement_date = parse_date_CURIE(.data$settlement_date, "settlement_date"),
      maturity_date = parse_date_CURIE(.data$maturity_date, "maturity_date"),
      clean_price = parse_number_CURIE(.data$clean_price, "clean_price"),
      coupon_rate = parse_number_CURIE(.data$coupon_rate, "coupon_rate"),
      coupon_frequency = parse_number_CURIE(.data$coupon_frequency, "coupon_frequency"),
      redemption = parse_number_CURIE(.data$redemption, "redemption"),
      ISIN = stringr::str_to_upper(stringr::str_trim(.data$ISIN)),
      price_type = stringr::str_to_lower(.data$price_type),
      jgb_price_id = paste("JGB_PRICE", .data$source_file, .data$marketdate, .data$ISIN, sep = "|")
    )
}

# 円債の複利利回り（%）。Part II 第1章と同じ GaussQuant 関数・同じ引数の与え方
# （coupon_rate は%、クリーン単価、利払回数と複利回数を指定）。
# 残存年数は関数内の規約（満年数＋端数ACT/365）で、銘柄属性の day_count は使わない。
compound_yield_YEN_CURIE <- function(settlement_date, maturity_date, coupon_rate, clean_price,
                                     redemption, coupon_frequency, compound_frequency) {
  GaussQuant::bond_compound_yield_from_price_YEN_GQL(
    start_date = as.character(settlement_date), end_date = as.character(maturity_date),
    coupon_rate = coupon_rate * 100, price = clean_price, redemption = redemption,
    coupon_frequency = coupon_frequency, compound_frequency = compound_frequency
  )
}

# JGB時価（単価と属性）から複利利回りを計算する
compute_jgb_yields_CURIE <- function(jgb, compound_frequency = 2) {
  jgb |>
    dplyr::mutate(
      result = purrr::pmap(
        list(.data$settlement_date, .data$maturity_date, .data$coupon_rate, .data$clean_price,
             .data$redemption, .data$coupon_frequency, .data$price_type),
        function(settle, maturity, coupon, price, redemption, freq, type) {
          if (is.na(coupon)) return(list(y = NA_real_, msg = "JGBの銘柄属性がない"))
          if (!identical(type, "clean")) return(list(y = NA_real_, msg = "単価がクリーン価格ではない"))
          tryCatch(
            list(y = compound_yield_YEN_CURIE(settle, maturity, coupon, price, redemption,
                                              freq, compound_frequency), msg = ""),
            error = function(e) list(y = NA_real_, msg = conditionMessage(e))
          )
        }
      ),
      compound_yield_pct = purrr::map_dbl(.data$result, "y"),
      yield_message = purrr::map_chr(.data$result, "msg"),
      compound_frequency = compound_frequency,
      remaining_years = purrr::map2_dbl(.data$settlement_date, .data$maturity_date,
                                        \(s, m) GaussQuant::bond_year_fraction_YEN_GQL(s, m))
    ) |>
    dplyr::select(-"result")
}

# 同一の市場日・受渡日・償還日・計算規約のJGB複利利回りを算術平均する（補間の観測点）
average_jgb_yields_CURIE <- function(jgb_yields) {
  jgb_yields |>
    dplyr::filter(!is.na(.data$compound_yield_pct)) |>
    dplyr::summarise(
      jgb_yield_pct_mean = mean(.data$compound_yield_pct),
      n_jgbs = dplyr::n(),
      jgb_isins = paste(sort(.data$ISIN), collapse = ", "),
      quote_times = paste(sort(unique(stats::na.omit(.data$quote_time))), collapse = ", "),
      source_file = paste(sort(unique(.data$source_file)), collapse = ", "),
      .by = c("marketdate", "settlement_date", "maturity_date", "coupon_frequency", "compound_frequency")
    ) |>
    dplyr::arrange(.data$marketdate, .data$settlement_date, .data$maturity_date)
}

# 対象JPY債と同じ償還日のJGB複利利回りを補間し、JGBスプレッド（bp）を求める。
# JGBスプレッド = （対象債の複利利回り − 補間JGB複利利回り）× 100（利回りは%表示）
# 比較できる条件：同じ市場日、同じ受渡日（T+1単価をT+2として使わない）、
#                 同じ利払回数・複利回数、同じ計算関数（compound_yield_YEN_CURIE）
jgb_spread_CURIE <- function(jgb_avg, marketdate, settlement_date, maturity_date, target_yield_pct,
                             coupon_frequency, compound_frequency) {
  same_day <- dplyr::filter(jgb_avg, .data$marketdate == .env$marketdate)
  pts <- dplyr::filter(same_day, .data$settlement_date == .env$settlement_date,
                       .data$coupon_frequency == .env$coupon_frequency,
                       .data$compound_frequency == .env$compound_frequency)
  result <- function(value, message) {
    tibble::tibble(
      jgb_interpolated_yield_pct = value,
      jgb_spread_bp = (target_yield_pct - value) * 100,
      jgb_n_points = nrow(pts),
      jgb_marketdate = if (nrow(pts)) marketdate else as.Date(NA),
      jgb_quote_time = paste(unique(pts$quote_times[nzchar(pts$quote_times)]), collapse = ", "),
      jgb_curve_id = paste("JGB_YIELD", paste(unique(pts$source_file), collapse = ","), marketdate,
                           paste0("settle=", settlement_date), sep = "|"),
      jgb_spread_message = message
    )
  }
  if (!nrow(pts)) {
    settles <- paste(unique(same_day$settlement_date), collapse = ", ")
    return(result(NA_real_, paste0(
      "受渡日 ", settlement_date, "・利払年", coupon_frequency, "回・複利年", compound_frequency,
      "回に合うJGB利回りがない（JGB単価の受渡日: ", if (nzchar(settles)) settles else "なし",
      "。受渡日の違う単価は使わない）")))
  }
  if (is.na(target_yield_pct)) return(result(NA_real_, "対象債の複利利回りがない"))
  fit <- spline_by_maturity_CURIE(pts$maturity_date, pts$jgb_yield_pct_mean, settlement_date,
                                  maturity_date, "JGB利回り")
  result(fit$value, fit$message)
}
