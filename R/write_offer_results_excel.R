# 計算結果のExcel出力（出力のみ。計算は R/compute_offer_pricing.R）。
# 元のオファー情報に、銘柄属性・入力検証・計算結果・使用した市場データを付けて書き出す。
# JPYとISOは別のExcelに出力する。元のオファーExcelと同じパスには書かない。
# スプレッドの単位はすべてbp（列名の末尾 _bp）。利回りは%（_pct）。

.result_columns_common_CURIE <- c(
  # 元のオファー情報
  "offer_book", "offer_source_file", "offer_source_sheet", "offer_source_row",
  "offer_date", "ISIN", "offer_security_name", "broker_names", "offer_currency",
  "offer_price", "offer_size", "offer_size_unit", "offer_settlementdate", "sheet_issue_price",
  # 銘柄属性
  "attribute_date", "attribute_source", "security_name", "currency", "coupon_type", "coupon_rate",
  "floating_index", "floating_spread_bp", "reset_frequency",
  "coupon_frequency", "day_count", "issue_date", "issue_price",
  "maturity_date", "first_call_date", "call_price", "redemption",
  "calculation_end_date", "calculation_end_basis",
  # 入力検証
  "validation_status", "validation_message",
  # 計算条件と自国通貨OIS ASW
  "valuation_date", "bond_settlement_date", "asw_effective_date", "asw_maturity_date",
  "accrued_per_100", "dirty_price", "asw_payment_frequency", "asw_payment_lag", "asw_conditions_note",
  "native_asw_bp", "native_ois_curve_id", "native_ois_marketdate", "native_ois_quote_time"
)

.result_columns_workflow_CURIE <- list(
  JPY = c("jpy_compound_yield_pct", "jgb_interpolated_yield_pct", "jgb_spread_bp",
          "jgb_yield_convention", "jgb_curve_id", "jgb_marketdate", "jgb_quote_time", "jgb_spread_status"),
  ISO = c("fx_spot", "fx_spot_date", "fx_quote_id", "fx_marketdate", "fx_quote_time",
          "basis_bp", "basis_quote_set_id", "basis_method", "basis_marketdate", "basis_quote_time",
          "jpy_asw_bp", "simple_sum_bp", "calibrated_discount_spread_bp",
          "ccs_repricing_npv_jpy", "fair_spread_npv_jpy",
          "jpy_ois_curve_id", "jpy_ois_marketdate", "jpy_ois_quote_time", "jpy_asw_model", "jpy_asw_status")
)

result_column_notes_CURIE <- function(workflow) {
  common <- tibble::tribble(
    ~列,                               ~内容,
    "offer_source_file/sheet/row",    "元のオファーExcelのファイル・シート・行番号",
    "offer_price",                    "オファー単価。額面100当たりのクリーン価格",
    "offer_size",                     "額面金額（通貨単位）。offer_size_unit は元の表示単位",
    "issue_price",                    "発行単価（銘柄属性）。オファー単価とは別の項目",
    "maturity_date / first_call_date", "最終償還日とFirst Call日（別項目）",
    "calculation_end_date/basis",     "計算終期とその根拠。現在は固定利付・満期一括・償還額100の最終償還日のみ",
    "validation_status/message",      "入力検証の結果と問題コード",
    "valuation_date",                 "評価日（＝オファー日＝各市場データの市場日）",
    "bond_settlement_date",           "債券の受渡日（評価日＋受渡日数）。オファー受渡日と一致した行だけ計算",
    "asw_payment_frequency/lag",      "ASWの翌日物レッグの払い方。現段階ではデモの仮定（設定で変更可）",
    "native_asw_bp",                  "建て通貨のOISに対するOIS ASW（bp）。compute_asset_swap_ISO()",
    "*_marketdate / *_quote_time",    "使用した市場データの市場日と取得時刻（ファイルに取得時刻がある場合）",
    "*_curve_id / *_quote_id / *_set_id", "使用した市場データの識別情報（種類|ファイル|市場日|銘柄等）",
    "pricing_status",                 "success＝全指標計算、partial＝一部未計算、failed＝計算失敗、unsupported＝商品が計算関数の対象外、skipped＝入力検証で不合格",
    "pricing_message",                "未計算・失敗・未対応の理由"
  )
  specific <- if (workflow == "JPY") {
    tibble::tribble(
      ~列,                           ~内容,
      "jpy_compound_yield_pct",     "対象JPY債の複利利回り（%）。オファー単価から計算",
      "jgb_interpolated_yield_pct", "同じ償還日に補間したJGB複利利回り（%）",
      "jgb_spread_bp",              "JGBスプレッド（bp）＝（jpy_compound_yield_pct − jgb_interpolated_yield_pct）×100",
      "jgb_yield_convention",       "対象債・JGBに共通の複利利回りの計算規約"
    )
  } else {
    tibble::tribble(
      ~列,                              ~内容,
      "fx_spot",                       "外貨1単位当たりの円。ASW開始日の交換レートとして使用",
      "basis_bp",                      "円レッグに加算する通貨ベーシス（bp）。気配の平均を自然3次スプライン補間",
      "jpy_asw_bp",                    "円換算ASW（bp）。円担保・TONAR基準。既存の簡易モデル（jpy_asw_model）",
      "simple_sum_bp",                 "native_asw_bp + basis_bp（参考値。評価値は jpy_asw_bp）",
      "calibrated_discount_spread_bp", "ベーシス1点から校正した外貨割引の一定スプレッド（bp）",
      "ccs_repricing_npv_jpy / fair_spread_npv_jpy", "校正・フェア条件のNPV残差（円、額面100当たりの取引）"
    )
  }
  dplyr::bind_rows(common, specific)
}

.notes_CURIE <- function(workflow, results, settings) {
  status_count <- table(results$pricing_status)
  common <- c(
    "ワークフロー" = if (workflow == "JPY") "JPY（建て通貨が円。ユーロ円債を含む）" else "ISO（建て通貨が円以外）",
    "データ" = "デモ実行時は架空のテストデータ。実在の銘柄・市場価格ではない",
    "評価日" = paste(format(sort(unique(results$valuation_date))), collapse = ", "),
    "単位" = "スプレッドはbp（列名の末尾 _bp）、利回りは%（_pct）、価格は額面100当たり",
    "市場データの日付" = "評価日と同じ市場日のデータだけを使う。不一致・不足を別の日付で補わない（「市場データ」シート）",
    "自国ASW" = "compute_asset_swap_ISO()：債券CFを支払い、翌日物＋スプレッドを受け取る。オファー単価はクリーン価格",
    "ASWの払い方" = paste0("翌日物レッグ ", settings$asw_payment_frequency, "、支払ラグ ", settings$asw_payment_lag,
                      "。", settings$assumption_note, "（pricing_settings_CURIE() で変更可）"),
    "計算終期" = "最終償還日。変動利付・期限前償還条項付き等は計算関数の対象外とし、固定利付債として代用しない（unsupported）"
  )
  specific <- if (workflow == "JPY") {
    c(
      "JGBスプレッド" = paste("（対象JPY債の複利利回り − 同じ償還日のJGB複利利回り）×100 bp。",
                             "JGB利回りは同一市場日・同一受渡日・同一償還日を算術平均し、残存年数で自然3次スプライン補間（外挿なし）"),
      "複利利回りの規約" = paste0("対象債・JGBとも GaussQuant::bond_compound_yield_from_price_YEN_GQL()、クリーン単価、同じ受渡日、",
                          "複利年", settings$jgb_compound_frequency, "回。受渡日・利払回数・複利回数が違うJGBは比較に使わない")
    )
  } else {
    c(
      "円換算ASW" = .jpy_asw_model_note_CURIE,
      "通貨ベーシス" = paste("外貨OISフラット支払／円TONAR＋basis受取。同一の通貨ペア・市場日・契約条件・償還日の気配を算術平均し、",
                         "残存年数で自然3次スプライン補間（外挿なし）。ベーシス気配の補間であり、CCSカーブのブートストラップではない"),
      "FXの向き" = "fx_spot は外貨1単位当たりの円。ASW開始日の交換レートとして使い、受渡日が開始日と違う場合は計算しない"
    )
  }
  items <- c(common, specific,
             "集計" = paste0(nrow(results), "件（", paste(names(status_count), status_count, sep = " ", collapse = "、"), "）"))
  tibble::tibble(項目 = names(items), 内容 = unname(items))
}

# market_sheets : 市場データの一覧（JPY：JGB利回りなど、ISO：通貨ベーシスなど）の名前付きlist
write_offer_results_excel_CURIE <- function(workflow = c("JPY", "ISO"), pricing, market_log, attributes,
                                            market_sheets, environment, gaussquant_check, path,
                                            protected_paths = character()) {
  workflow <- match.arg(workflow)
  target <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (target %in% normalizePath(protected_paths, winslash = "/", mustWork = FALSE)) {
    stop("出力先が入力ファイルと同じです。元のExcelは上書きしません: ", path, call. = FALSE)
  }
  columns <- c(.result_columns_common_CURIE, .result_columns_workflow_CURIE[[workflow]],
               "pricing_status", "pricing_message")
  results <- pricing$results |>
    dplyr::select(dplyr::any_of(columns)) |>
    dplyr::mutate(offer_source_file = basename(.data$offer_source_file))
  summary <- results |>
    dplyr::count(offer_book, currency, validation_status, pricing_status, name = "件数")
  # list列は書けないため、属性の記録は表の列だけにする
  attribute_sheet <- dplyr::select(attributes, dplyr::where(\(x) !is.list(x)))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writexl::write_xlsx(
    c(
      list(
        "説明" = .notes_CURIE(workflow, results, pricing$settings),
        "列の説明" = result_column_notes_CURIE(workflow),
        "オファー結果" = results,
        "集計" = summary,
        "市場データ" = market_log,
        "OISカーブ" = pricing$curves,
        "銘柄属性" = attribute_sheet
      ),
      market_sheets,
      list("実行環境" = environment, "GaussQuant照合" = gaussquant_check)
    ),
    path
  )
  invisible(path)
}
