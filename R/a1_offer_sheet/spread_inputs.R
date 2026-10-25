# オファーシートのスプレッド計算に使う市場データの前処理（A1）。
# 通貨ベーシス：同一条件・同一償還日の平均と補間。JGB：同一償還日の平均と補間、JGBスプレッド。


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

