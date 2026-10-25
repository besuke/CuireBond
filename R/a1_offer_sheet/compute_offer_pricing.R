# 検証済みオファーの価格計算（計算のみ。Excel出力は R/a1_offer_sheet/write_offer_results_excel.R）。
#
# 使う既存関数
# - OISカーブ   : instance_swap_curve()（R/z0_utility/lagrange/instance_swap_curve.R）
# - 自国ASW     : compute_asset_swap_ISO()（R/z0_utility/lagrange/compute_asset_swap_ISO.R）
# - 円換算ASW   : compute_asset_swap_basis_JPY()（R/z0_utility/lagrange/compute_asset_swap_basis_JPY.R）
# - 債券の生成  : GaussQuant::fixed_rate_bond_GQL()、GaussQuant::day_counter_GQL()
# - 円債の利回り: GaussQuant::bond_compound_yield_from_price_YEN_GQL()（R/z0_utility/market_data_import.R 経由）
#
# 計算条件
# - 評価日 = オファー日 = 各市場データの市場日。日付が違うデータでは補わない
# - 既存ASW関数は債券の受渡日（Bond_settlementDate）を開始日とするため、それが
#   オファーの受渡日と一致することを確かめ、違えば計算しない
# - 計算終期は calculation_end_date（現在は最終償還日。固定利付・満期一括・償還額100のみ）
# - ASWの翌日物レッグの払い方は pricing_settings_CURIE() で指定する。
#   既定の半年払い・支払ラグ0は、現段階ではデモの仮定（既存関数の既定値と同じ）
# - 円換算ASWは既存の簡易モデル（.jpy_asw_model_note_CURIE を参照）
# - fx_spot は「ASW開始日に交換するレート」。FXの受渡日が開始日と違う行は計算しない
# - JGBスプレッド = 対象JPY債の複利利回り − 同じ償還日に補間したJGB複利利回り（bp）

.jpy_asw_model_note_CURIE <- paste(
  "既存の簡易モデル（compute_asset_swap_basis_JPY）：円担保・元本固定・開始日と満期に元本交換。",
  "満期に対応するベーシス1点（気配を補間した値）で外貨割引の一定スプレッドを校正する。",
  "CCSの期間構造全体を校正したモデルではない。MtMリセット・FXスワップ調整・取引費用・",
  "クオント調整・信用等の調整は含まない"
)

# 計算設定。ASWの払い方は、現段階ではデモの仮定として半年払い・支払ラグ0を既定にする。
pricing_settings_CURIE <- function(
    asw_payment_frequency = "Semiannual", asw_payment_lag = 0L,
    jpy_payment_frequency = "Semiannual", jpy_payment_lag = 0L,
    jgb_compound_frequency = 2,
    assumption_note = "ASWの翌日物レッグ（半年払い・支払ラグ0）はデモの仮定") {
  list(
    asw_payment_frequency = asw_payment_frequency, asw_payment_lag = as.integer(asw_payment_lag),
    jpy_payment_frequency = jpy_payment_frequency, jpy_payment_lag = as.integer(jpy_payment_lag),
    jgb_compound_frequency = jgb_compound_frequency, assumption_note = assumption_note
  )
}

.empty_pricing_CURIE <- function(settings) {
  tibble::tibble(
    valuation_date = as.Date(NA), bond_settlement_date = as.Date(NA), asw_effective_date = as.Date(NA),
    asw_maturity_date = as.Date(NA), accrued_per_100 = NA_real_, dirty_price = NA_real_,
    asw_payment_frequency = settings$asw_payment_frequency, asw_payment_lag = settings$asw_payment_lag,
    asw_conditions_note = settings$assumption_note,
    native_asw_bp = NA_real_, native_asw_status = NA_character_,
    native_ois_curve_id = NA_character_, native_ois_marketdate = as.Date(NA), native_ois_quote_time = NA_character_,
    jpy_compound_yield_pct = NA_real_, jgb_interpolated_yield_pct = NA_real_, jgb_spread_bp = NA_real_,
    jgb_yield_convention = NA_character_, jgb_curve_id = NA_character_,
    jgb_marketdate = as.Date(NA), jgb_quote_time = NA_character_, jgb_spread_status = NA_character_,
    fx_spot = NA_real_, fx_spot_date = as.Date(NA), fx_quote_id = NA_character_,
    fx_marketdate = as.Date(NA), fx_quote_time = NA_character_,
    basis_bp = NA_real_, basis_quote_set_id = NA_character_, basis_method = NA_character_,
    basis_marketdate = as.Date(NA), basis_quote_time = NA_character_,
    jpy_asw_bp = NA_real_, simple_sum_bp = NA_real_, calibrated_discount_spread_bp = NA_real_,
    ccs_repricing_npv_jpy = NA_real_, fair_spread_npv_jpy = NA_real_,
    jpy_asw_model = NA_character_, jpy_asw_status = NA_character_,
    jpy_ois_curve_id = NA_character_, jpy_ois_marketdate = as.Date(NA), jpy_ois_quote_time = NA_character_,
    pricing_status = NA_character_, pricing_message = NA_character_
  )
}

# JPY債：JGBスプレッド（対象債とJGBは同じ関数・同じ受渡日・同じ利払回数と複利回数で計算）
.price_jgb_spread_CURIE <- function(out, row, jgb_avg, settings) {
  out$jpy_compound_yield_pct <- compound_yield_YEN_CURIE(
    row$offer_settlementdate, row$calculation_end_date, row$coupon_rate, row$offer_price,
    row$redemption, row$coupon_frequency, settings$jgb_compound_frequency
  )
  out$jgb_yield_convention <- paste0(
    "GaussQuant::bond_compound_yield_from_price_YEN_GQL、クリーン単価、受渡日 ",
    row$offer_settlementdate, "、利払年", row$coupon_frequency, "回・複利年",
    settings$jgb_compound_frequency, "回、残存年数は満年数＋端数ACT/365（対象債・JGB共通）")
  spread <- jgb_spread_CURIE(jgb_avg, row$offer_date, row$offer_settlementdate, row$calculation_end_date,
                             out$jpy_compound_yield_pct, row$coupon_frequency, settings$jgb_compound_frequency)
  out$jgb_interpolated_yield_pct <- spread$jgb_interpolated_yield_pct
  out$jgb_spread_bp <- spread$jgb_spread_bp
  out$jgb_curve_id <- spread$jgb_curve_id
  out$jgb_marketdate <- spread$jgb_marketdate
  out$jgb_quote_time <- spread$jgb_quote_time
  out$jgb_spread_status <- if (is.na(spread$jgb_spread_bp)) "not_computed" else "computed"
  list(out = out, message = if (is.na(spread$jgb_spread_bp)) paste0("JGBスプレッド未計算: ", spread$jgb_spread_message))
}

# 外貨債：円換算ASW（既存の簡易モデル）
.price_jpy_asw_CURIE <- function(out, row, bond, native, curve, curves, fx, basis_avg, settings) {
  ccy <- row$currency
  out$jpy_asw_status <- "not_computed"
  out$jpy_asw_model <- .jpy_asw_model_note_CURIE
  jpy_curve <- curves[["JPY"]]
  fx_row <- dplyr::filter(fx, .data$marketdate == row$offer_date,
                          .data$base_currency == ccy, .data$quote_currency == "JPY")
  basis_row <- interpolate_ccs_basis_CURIE(basis_avg, row$offer_date, row$calculation_end_date, ccy,
                                           basis_terms_for_model_CURIE(ccy, curve$index_name))
  out$basis_bp <- basis_row$basis_bp
  out$basis_quote_set_id <- basis_row$basis_quote_set_id
  out$basis_method <- basis_row$basis_method
  out$basis_marketdate <- basis_row$basis_marketdate
  out$basis_quote_time <- basis_row$basis_quote_time
  if (nrow(fx_row) == 1L) {
    out$fx_spot <- fx_row$fx_spot
    out$fx_spot_date <- fx_row$spot_date
    out$fx_quote_id <- fx_row$fx_quote_id
    out$fx_marketdate <- fx_row$marketdate
    out$fx_quote_time <- fx_row$quote_time
  }
  reason <- if (is.null(jpy_curve) || !is.null(jpy_curve$error)) {
    paste0("JPYのOISカーブを構築できない: ", jpy_curve$error)
  } else if (nrow(fx_row) != 1L) {
    paste0("評価日の対円FXスポットが1件に定まらない（", nrow(fx_row), "件）")
  } else if (fx_row$spot_date != native$effective_date) {
    paste0("FXの受渡日 ", fx_row$spot_date, " とASW開始日 ", native$effective_date,
           " が異なる（FXスワップ調整は未実装）")
  } else if (is.na(basis_row$basis_bp)) {
    basis_row$basis_message
  } else {
    ""
  }
  if (nzchar(reason)) return(list(out = out, message = paste0("円換算ASW未計算: ", reason)))
  jpy <- compute_asset_swap_basis_JPY(
    bond = bond, clean_price = row$offer_price,
    foreign_curve = curve, jpy_curve = jpy_curve,
    fx_spot = out$fx_spot, basis_bp = out$basis_bp, currency = ccy,
    foreign_payment_frequency = settings$asw_payment_frequency,
    jpy_payment_frequency = settings$jpy_payment_frequency,
    foreign_payment_lag = settings$asw_payment_lag,
    jpy_payment_lag = settings$jpy_payment_lag
  )
  v <- jpy$valuation
  out$jpy_asw_bp <- v$jpy_asw_bp
  out$simple_sum_bp <- v$simple_sum_bp
  out$calibrated_discount_spread_bp <- v$calibrated_discount_spread_bp
  out$ccs_repricing_npv_jpy <- v$ccs_repricing_npv_jpy
  out$fair_spread_npv_jpy <- v$fair_spread_npv_jpy
  out$jpy_asw_status <- "computed"
  out$jpy_ois_curve_id <- jpy_curve$curve_id
  out$jpy_ois_marketdate <- jpy_curve$marketdate
  out$jpy_ois_quote_time <- jpy_curve$quote_time
  list(out = out, message = NULL)
}

# オファー1行の計算。各指標に「計算済み／未計算」と理由を付ける。
price_offer_row_CURIE <- function(workflow, row, curves, market, basis_avg, jgb_avg, settings) {
  out <- .empty_pricing_CURIE(settings)
  out$valuation_date <- row$offer_date
  fail <- function(message) {
    out$pricing_status <- "failed"
    out$pricing_message <- message
    out
  }
  if (!identical(row$validation_status, "valid")) {
    if (isTRUE(row$unsupported_only)) {
      out$pricing_status <- "unsupported"
      out$pricing_message <- paste0("未対応: ", dplyr::coalesce(row$unsupported_reason, row$validation_message),
                                    "（固定利付債として代用しない）")
    } else {
      out$pricing_status <- "skipped"
      out$pricing_message <- paste("入力検証で不合格:", row$validation_message)
    }
    return(out)
  }
  curve <- curves[[row$currency]]
  if (is.null(curve) || !is.null(curve$error)) {
    return(fail(paste0(row$currency, " のOISカーブを構築できない: ", curve$error)))
  }
  run <- function() {
    bond <- build_fixed_rate_bond_CURIE(row)
    # 評価日をオファー日にし、債券の受渡日がオファーの受渡日と一致するか確かめる
    GaussQuant::set_eval_date_GQL(GaussQuant::date_GQL(as.character(row$offer_date)))
    out$bond_settlement_date <- as.Date(GaussQuant::safe_iso_GQH(QuantLib::Bond_settlementDate(bond)))
    if (out$bond_settlement_date != row$offer_settlementdate) {
      return(fail(paste0("受渡日の不一致: 債券の受渡日 ", out$bond_settlement_date,
                         "（評価日+", row$settlement_days, "営業日）とオファー受渡日 ",
                         row$offer_settlementdate, " が異なる")))
    }
    # 自国通貨のOIS ASW
    native <- compute_asset_swap_ISO(bond, row$offer_price, curve,
                                     payment_frequency = settings$asw_payment_frequency,
                                     payment_lag = settings$asw_payment_lag)
    if (native$maturity_date != row$calculation_end_date) {
      return(fail(paste0("ASWの満期 ", native$maturity_date, " が計算終期 ", row$calculation_end_date, " と異なる")))
    }
    out$asw_effective_date <- native$effective_date
    out$asw_maturity_date <- native$maturity_date
    out$accrued_per_100 <- native$valuation$accrued_per_100
    out$dirty_price <- native$valuation$dirty_price
    out$native_asw_bp <- native$valuation$fair_spread_bp
    out$native_asw_status <- "computed"
    out$native_ois_curve_id <- curve$curve_id
    out$native_ois_marketdate <- curve$marketdate
    out$native_ois_quote_time <- curve$quote_time
    step <- if (workflow == "JPY") {
      .price_jgb_spread_CURIE(out, row, jgb_avg, settings)
    } else {
      .price_jpy_asw_CURIE(out, row, bond, native, curve, curves, market$fx, basis_avg, settings)
    }
    out <- step$out
    out$pricing_status <- if (length(step$message)) "partial" else "success"
    out$pricing_message <- if (length(step$message)) step$message else ""
    out
  }
  tryCatch(run(), error = function(e) fail(paste("計算エラー:", conditionMessage(e))))
}

# 検証済みワークシート全体の計算（評価日は1つ）。
# workflow = "JPY"：JPY OIS ASW と JGBスプレッド。market = list(jgb_yields = ...)
# workflow = "ISO"：自国OIS ASW と円換算ASW。market = list(fx = ..., basis = ...)
compute_offer_pricing_CURIE <- function(workflow = c("JPY", "ISO"), worksheet, ois, market, valuation_date,
                                        settings = pricing_settings_CURIE()) {
  workflow <- match.arg(workflow)
  basis_avg <- if (workflow == "ISO") average_ccs_basis_CURIE(market$basis) else NULL
  jgb_avg <- if (workflow == "JPY") average_jgb_yields_CURIE(market$jgb_yields) else NULL
  valid_ccy <- stats::na.omit(worksheet$currency[worksheet$validation_status == "valid"])
  needed <- unique(c("JPY", valid_ccy))
  curves <- build_ois_curves_CURIE(ois, valuation_date, needed)
  results <- purrr::map(seq_len(nrow(worksheet)), \(i) {
    price_offer_row_CURIE(workflow, worksheet[i, ], curves, market, basis_avg, jgb_avg, settings)
  }) |> purrr::list_rbind()
  list(
    results = worksheet |>
      dplyr::select(-dplyr::any_of(names(results))) |>
      dplyr::bind_cols(results),
    curves = summarise_ois_curves_CURIE(curves),
    basis_average = basis_avg,
    jgb_average = jgb_avg,
    settings = settings
  )
}

