# A1の結果をHTMLで確認するための表（POC・レポート用）。
# 入力は workflows_offer_sheet_JPY() / workflows_offer_sheet_ISO() の戻り値で、
# Excelを読み直さない。計算はせず、表示用に整形するだけ。
# 各関数は flextable を返す（R/z0_utility/report_format.R の flextable_Curie）。

.a1_security_name_Curie <- function(r) dplyr::coalesce(r$security_name, r$offer_security_name)

# 評価条件：入力ファイル・市場日・取得時刻・評価日・受渡日
a1_conditions_table_Curie <- function(run, workflow) {
  r <- run$results
  inputs <- tibble::tibble(
    区分 = "オファー", ファイル = unique(basename(r$offer_source_file)),
    市場日 = paste(format_date_Curie(sort(unique(r$offer_date))), collapse = ", "),
    取得時刻 = "—", 状態 = "使用"
  )
  market <- run$market_log |>
    dplyr::transmute(区分 = .data$data_type, ファイル = .data$source_file, 市場日 = .data$marketdates,
                     取得時刻 = dplyr::if_else(nzchar(.data$quote_times), .data$quote_times, "—"),
                     状態 = .data$status)
  attrs <- tibble::tibble(
    区分 = "銘柄属性", ファイル = paste(unique(stats::na.omit(run$attributes$attribute_source)), collapse = ", "),
    市場日 = paste(format_date_Curie(sort(unique(run$attributes$attribute_date))), collapse = ", "),
    取得時刻 = paste(unique(stats::na.omit(run$attributes$attribute_retrieved_at)), collapse = ", "),
    状態 = "使用"
  )
  conditions <- tibble::tibble(
    区分 = c("評価日", "オファー受渡日", "ASW翌日物レッグ"),
    ファイル = c(paste(format_date_Curie(sort(unique(r$valuation_date))), collapse = ", "),
             paste(format_date_Curie(sort(unique(r$offer_settlementdate))), collapse = ", "),
             paste0(run$pricing$settings$asw_payment_frequency, "、支払ラグ ", run$pricing$settings$asw_payment_lag)),
    市場日 = "—", 取得時刻 = "—",
    状態 = c("オファー日・市場日と一致させる", "債券の受渡日と一致した行だけ計算", run$pricing$settings$assumption_note)
  )
  dplyr::bind_rows(inputs, market, attrs, conditions) |>
    flextable_Curie(widths = c(区分 = 1.2, ファイル = 2.6, 市場日 = 1, 取得時刻 = 1.9, 状態 = 2.4),
                    caption = paste0(workflow, "：入力ファイルと評価条件"))
}

# 入力件数・検証状態・計算状態の集計
a1_status_summary_table_Curie <- function(run, workflow) {
  run$results |>
    dplyr::count(.data$currency, .data$validation_status, .data$pricing_status, name = "件数") |>
    dplyr::transmute(通貨 = .data$currency,
                     入力検証 = validation_status_label_Curie(.data$validation_status),
                     計算状態 = pricing_status_label_Curie(.data$pricing_status), 件数 = .data$件数) |>
    .add_total_row_Curie() |>
    flextable_Curie(right = "件数", widths = c(通貨 = 0.8, 入力検証 = 1, 計算状態 = 1.1, 件数 = 0.7),
                    caption = paste0(workflow, "：入力件数・検証状態・計算状態"))
}

.add_total_row_Curie <- function(x) {
  dplyr::bind_rows(dplyr::mutate(x, 件数 = as.character(.data$件数)),
                   tibble::tibble(通貨 = "合計", 入力検証 = "", 計算状態 = "", 件数 = as.character(sum(x$件数))))
}

# JPY：銘柄名、ISIN、オファー単価、複利利回り、OIS ASW、JGBスプレッド、計算状態
a1_jpy_results_table_Curie <- function(run) {
  r <- run$results
  ok <- r$pricing_status == "success"
  tibble::tibble(
    銘柄名 = .a1_security_name_Curie(r), ISIN = r$ISIN,
    オファー単価 = format_number_Curie(r$offer_price, 2),
    複利利回り = format_number_Curie(r$jpy_compound_yield_pct, 4, ok),
    補間JGB利回り = format_number_Curie(r$jgb_interpolated_yield_pct, 4, ok),
    OIS_ASW = format_number_Curie(r$native_asw_bp, 1, ok),
    JGBスプレッド = format_number_Curie(r$jgb_spread_bp, 1, ok),
    計算状態 = pricing_status_label_Curie(r$pricing_status)
  ) |>
    flextable_Curie(
      labels = c(オファー単価 = "オファー単価\n（額面100・クリーン）", 複利利回り = "複利利回り\n（%）",
                 補間JGB利回り = "補間JGB利回り\n（%）", OIS_ASW = "JPY OIS ASW\n（bp）",
                 JGBスプレッド = "JGBスプレッド\n（bp）"),
      right = c("オファー単価", "複利利回り", "補間JGB利回り", "OIS_ASW", "JGBスプレッド"),
      widths = c(銘柄名 = 2.4, ISIN = 1.2, オファー単価 = 1, 複利利回り = 0.9, 補間JGB利回り = 1,
                 OIS_ASW = 0.9, JGBスプレッド = 0.9, 計算状態 = 0.8),
      caption = "JPY：計算結果（スプレッドの単位はbp）")
}

# ISO：銘柄名、ISIN、通貨、オファー単価、自国通貨OIS ASW、円換算ASW、計算状態
a1_iso_results_table_Curie <- function(run) {
  r <- run$results
  ok <- r$pricing_status == "success"
  tibble::tibble(
    銘柄名 = .a1_security_name_Curie(r), ISIN = r$ISIN, 通貨 = r$currency,
    オファー単価 = format_number_Curie(r$offer_price, 2),
    自国OIS_ASW = format_number_Curie(r$native_asw_bp, 1, ok),
    ベーシス = format_number_Curie(r$basis_bp, 2, ok),
    円換算ASW = format_number_Curie(r$jpy_asw_bp, 1, ok),
    単純合算 = format_number_Curie(r$simple_sum_bp, 1, ok),
    計算状態 = pricing_status_label_Curie(r$pricing_status)
  ) |>
    flextable_Curie(
      labels = c(オファー単価 = "オファー単価\n（額面100・クリーン）", 自国OIS_ASW = "自国通貨\nOIS ASW（bp）",
                 ベーシス = "補間ベーシス\n（bp）", 円換算ASW = "円換算ASW\n（bp）", 単純合算 = "参考：単純合算\n（bp）"),
      right = c("オファー単価", "自国OIS_ASW", "ベーシス", "円換算ASW", "単純合算"),
      widths = c(銘柄名 = 2.4, ISIN = 1.2, 通貨 = 0.5, オファー単価 = 1, 自国OIS_ASW = 0.9,
                 ベーシス = 0.9, 円換算ASW = 0.9, 単純合算 = 1, 計算状態 = 0.8),
      caption = "ISO：計算結果（スプレッドの単位はbp。評価値は円換算ASW、単純合算は参考）")
}

# 未対応・失敗・一部未計算・入力不備の銘柄と理由
a1_exceptions_table_Curie <- function(run, workflow) {
  r <- dplyr::filter(run$results, .data$pricing_status != "success")
  if (!nrow(r)) {
    r <- tibble::tibble(銘柄名 = "（該当なし）", ISIN = "", 計算状態 = "", 入力検証の問題コード = "", 理由 = "")
  } else {
    r <- tibble::tibble(
      銘柄名 = .a1_security_name_Curie(r), ISIN = r$ISIN,
      計算状態 = pricing_status_label_Curie(r$pricing_status),
      入力検証の問題コード = dplyr::if_else(nzchar(r$validation_message), r$validation_message, "—"),
      理由 = r$pricing_message
    )
  }
  flextable_Curie(r, widths = c(銘柄名 = 2.2, ISIN = 1.2, 計算状態 = 0.8, 入力検証の問題コード = 1.6, 理由 = 3.4),
                  caption = paste0(workflow, "：未対応・失敗した銘柄と理由"))
}

# JGB：単価から計算した複利利回り
a1_jgb_yields_table_Curie <- function(run) {
  run$jgb_yields |>
    dplyr::transmute(
      銘柄名 = .data$security_name, ISIN = .data$ISIN, 償還日 = format_date_Curie(.data$maturity_date),
      受渡日 = format_date_Curie(.data$settlement_date),
      クーポン = format_number_Curie(.data$coupon_rate * 100, 2),
      単価 = format_number_Curie(.data$clean_price, 3),
      残存年数 = format_number_Curie(.data$remaining_years, 4),
      複利利回り = format_number_Curie(.data$compound_yield_pct, 4, .data$yield_message == ""),
      備考 = dplyr::if_else(nzchar(.data$yield_message), .data$yield_message, "—")
    ) |>
    flextable_Curie(
      labels = c(クーポン = "クーポン\n（%）", 単価 = "単価\n（クリーン）", 複利利回り = "複利利回り\n（%）"),
      right = c("クーポン", "単価", "残存年数", "複利利回り"),
      widths = c(銘柄名 = 1.8, ISIN = 1.2, 償還日 = 0.9, 受渡日 = 0.9, クーポン = 0.7, 単価 = 0.8,
                 残存年数 = 0.8, 複利利回り = 0.9, 備考 = 1.2),
      caption = "JGB：単価から計算した複利利回り（複利年2回）")
}

# JGB：同一償還日の平均（補間の観測点）と、対象債の償還日での補間結果
a1_jgb_interpolation_table_Curie <- function(run) {
  points <- run$pricing$jgb_average |>
    dplyr::transmute(区分 = "観測点（平均）", 償還日 = format_date_Curie(.data$maturity_date),
                     受渡日 = format_date_Curie(.data$settlement_date), 銘柄数 = as.character(.data$n_jgbs),
                     JGB利回り = format_number_Curie(.data$jgb_yield_pct_mean, 4), 対象債 = .data$jgb_isins)
  r <- run$results
  ok <- r$pricing_status == "success"
  targets <- tibble::tibble(
    区分 = "対象債の償還日で補間", 償還日 = format_date_Curie(r$calculation_end_date),
    受渡日 = format_date_Curie(r$offer_settlementdate), 銘柄数 = "—",
    JGB利回り = format_number_Curie(r$jgb_interpolated_yield_pct, 4, ok),
    対象債 = paste0(.a1_security_name_Curie(r), "（", r$ISIN, "）")
  )
  dplyr::bind_rows(points, targets) |>
    flextable_Curie(labels = c(JGB利回り = "JGB複利利回り\n（%）", 対象債 = "JGB銘柄／対象債"),
                    right = c("銘柄数", "JGB利回り"),
                    widths = c(区分 = 1.6, 償還日 = 0.9, 受渡日 = 0.9, 銘柄数 = 0.6, JGB利回り = 1, 対象債 = 3.2),
                    caption = "JGB：同一償還日の平均と、自然3次スプラインによる補間（外挿なし）")
}

# 通貨ベーシス：同一条件・同一償還日の平均と、対象債の償還日での補間結果
a1_basis_table_Curie <- function(run) {
  points <- run$pricing$basis_average |>
    dplyr::transmute(区分 = "観測点（平均）", 通貨ペア = .data$currency_pair, 元本 = .data$notional_type,
                     償還日 = format_date_Curie(.data$maturity_date), 気配数 = as.character(.data$n_quotes),
                     ベーシス = format_number_Curie(.data$basis_bp_mean, 2), 気配元 = .data$quote_sources)
  r <- dplyr::filter(run$results, .data$currency != "JPY")
  ok <- r$pricing_status == "success"
  targets <- tibble::tibble(
    区分 = "対象債の償還日で補間", 通貨ペア = paste0(r$currency, "JPY"), 元本 = "constant",
    償還日 = format_date_Curie(r$calculation_end_date), 気配数 = "—",
    ベーシス = format_number_Curie(r$basis_bp, 2, ok),
    気配元 = paste0(.a1_security_name_Curie(r), "（", r$ISIN, "）")
  )
  dplyr::bind_rows(points, targets) |>
    flextable_Curie(labels = c(ベーシス = "ベーシス\n（bp）", 気配元 = "気配元／対象債"),
                    right = c("気配数", "ベーシス"),
                    widths = c(区分 = 1.6, 通貨ペア = 0.8, 元本 = 0.8, 償還日 = 0.9, 気配数 = 0.6,
                               ベーシス = 0.8, 気配元 = 2.6),
                    caption = paste("通貨ベーシス：同一条件・同一償還日の平均と、自然3次スプラインによる補間（外挿なし）。",
                                    "mtm_reset は現行モデルの条件に合わないため補間に使わない"))
}

# OISカーブの気配再現誤差
a1_curve_check_table_Curie <- function(run, workflow) {
  run$pricing$curves |>
    dplyr::transmute(
      通貨 = .data$currency, 状態 = dplyr::recode(.data$curve_status, success = "成功", failed = "失敗"),
      指標 = .data$index, 市場日 = format_date_Curie(.data$marketdate),
      取得時刻 = dplyr::coalesce(dplyr::na_if(.data$quote_time, ""), "—"),
      気配数 = as.character(.data$quotes),
      最大再現誤差 = formatC(.data$max_abs_quote_error_bp, format = "e", digits = 2),
      判定 = dplyr::if_else(.data$max_abs_quote_error_bp < 1e-4, "基準内（1e-4 bp 未満）", "基準超過")
    ) |>
    flextable_Curie(labels = c(最大再現誤差 = "最大再現誤差\n（bp）"),
                    right = c("気配数", "最大再現誤差"),
                    widths = c(通貨 = 0.6, 状態 = 0.6, 指標 = 0.8, 市場日 = 0.9, 取得時刻 = 1.9, 気配数 = 0.6,
                               最大再現誤差 = 1, 判定 = 1.6),
                    caption = paste0(workflow, "：OISカーブの気配再現誤差"))
}

# ISO：円換算ASWの校正・フェア条件の残差
a1_iso_residual_table_Curie <- function(run) {
  r <- dplyr::filter(run$results, .data$pricing_status == "success")
  tibble::tibble(
    銘柄名 = .a1_security_name_Curie(r), ISIN = r$ISIN, 通貨 = r$currency,
    FX = format_number_Curie(r$fx_spot, 2), FX受渡日 = format_date_Curie(r$fx_spot_date),
    ASW開始日 = format_date_Curie(r$asw_effective_date),
    校正スプレッド = format_number_Curie(r$calibrated_discount_spread_bp, 2),
    CCS残差 = formatC(r$ccs_repricing_npv_jpy, format = "e", digits = 2),
    フェア残差 = formatC(r$fair_spread_npv_jpy, format = "e", digits = 2)
  ) |>
    flextable_Curie(labels = c(FX = "FXスポット\n（円/外貨1単位）", 校正スプレッド = "校正した割引\nスプレッド（bp）",
                               CCS残差 = "CCS校正の\nNPV残差（円）", フェア残差 = "フェア条件の\nNPV残差（円）"),
                    right = c("FX", "校正スプレッド", "CCS残差", "フェア残差"),
                    widths = c(銘柄名 = 2.2, ISIN = 1.2, 通貨 = 0.5, FX = 1, FX受渡日 = 0.9, ASW開始日 = 0.9,
                               校正スプレッド = 1, CCS残差 = 1, フェア残差 = 1),
                    caption = "ISO：円換算ASWの校正・フェア条件の残差（額面100当たりの取引。基準は 1e-6 未満）")
}
