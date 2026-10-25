# c1：TRSターム条件の抽出項目の定義（指示書・照合・確認表・FpMLの共通の基準）。
#
# 区別すること
# - 参照資産・指数（return.*）と、資金調達レッグの参照金利（funding.floating_rate_index）は別物
# - 参照資産の評価日（return.valuation_*）と、資金調達金利のフィキシング／リセット日（funding.fixing_* / reset_*）は別項目
#
# type：text（文字）、code（コード。大文字で比較）、date（YYYY-MM-DD）、number（数値）、integer（整数）
# required：FpMLを作るのに必須か
#
# 値の状態（抽出JSONの value_status）。ドラフト・ひな形の未記入を、推測せず区別して記録する
#   stated    … 値が記載されている
#   blank     … 記入欄はあるが空欄（「[ ]」「______」「MMMM DD, YYYY」など）
#   bracketed … 角括弧の選択肢・既定案で、確定していない（例「[Modified Following]」）
#   not_found … 文書に該当する記載がない
.value_status_levels_c1 <- c("stated", "blank", "bracketed", "not_found")

# 確認表での値の状態の日本語表示
value_status_label_c1 <- function(x) {
  dplyr::recode(x, stated = "記載あり", blank = "空欄", bracketed = "選択肢（未確定）",
                not_found = "記載なし", .default = x, .missing = "—")
}

trs_field_dictionary_c1 <- function() {
  tibble::tribble(
    ~field,                                   ~section,   ~label_ja,                              ~type,     ~required,
    # 商品の種類（そもそもTRSか。文書が自分を何の取引と書いているか）
    "trade.product_type",                     "取引",     "商品の種類（TOTAL_RETURN_SWAP／OTHER）", "code",  TRUE,
    # 取引の基本条件
    "trade.dealer",                           "取引",     "証券会社",                             "text",    TRUE,
    "trade.dealer_reference",                 "取引",     "証券会社の取引番号",                   "text",    FALSE,
    "trade.trade_date",                       "取引",     "取引実行日（約定日）",                 "date",    TRUE,
    "trade.effective_date",                   "取引",     "効力発生日（開始日）",                 "date",    TRUE,
    "trade.termination_date",                 "取引",     "終了日",                               "date",    TRUE,
    "trade.notional_amount",                  "取引",     "想定元本",                             "number",  TRUE,
    "trade.notional_currency",                "取引",     "想定元本の通貨",                       "code",    TRUE,
    "trade.total_return_receiver",            "取引",     "トータル・リターンの受取者",           "text",    TRUE,
    "trade.total_return_payer",               "取引",     "トータル・リターンの支払者",           "text",    TRUE,
    "trade.calculation_agent",                "取引",     "計算代理人",                           "text",    FALSE,
    "trade.isda_definitions",                 "取引",     "準拠するISDA定義",                     "text",    TRUE,
    "trade.fx_initial_spot_rate",             "取引",     "当初スポットレート（為替）",           "number",  FALSE,
    "trade.fx_rate_quotation",                "取引",     "為替レートの表し方（例：IDR per USD）", "text",   FALSE,
    "trade.fx_hedge",                         "取引",     "為替ヘッジの有無・条件",               "text",    FALSE,
    # 参照資産（リターン・レッグ）
    "return.underlyer_type",                  "参照資産", "参照資産の種類（index／bond）",        "code",    TRUE,
    "return.underlyer_name",                  "参照資産", "参照資産・指数の名称",                 "text",    TRUE,
    "return.underlyer_id",                    "参照資産", "参照資産・指数の識別子",               "code",    TRUE,
    "return.underlyer_id_type",               "参照資産", "識別子の種類（ISIN／INDEX_CODE等）",   "code",    TRUE,
    "return.underlyer_currency",              "参照資産", "参照資産の通貨",                       "code",    FALSE,
    "return.underlyer_issuer",                "参照資産", "参照債券の発行体（Obligor）",          "text",    FALSE,
    "return.underlyer_maturity_date",         "参照資産", "参照債券の満期日",                     "date",    FALSE,
    "return.return_type",                     "参照資産", "リターンの種類（TOTAL／PRICE）",       "code",    TRUE,
    "return.initial_price",                   "参照資産", "当初価格",                             "number",  FALSE,
    "return.valuation_frequency",             "参照資産", "評価の頻度",                           "code",    TRUE,
    "return.valuation_date_rule",             "参照資産", "評価日の決め方（原文の要約）",         "text",    TRUE,
    "return.valuation_calendar",              "参照資産", "評価日の営業日カレンダー",             "code",    TRUE,
    "return.final_valuation_date",            "参照資産", "最終評価日",                           "date",    TRUE,
    "return.valuation_time",                  "参照資産", "評価時刻",                             "text",    FALSE,
    "return.income_treatment",                "参照資産", "利息・分配金の扱い",                   "text",    FALSE,
    # 資金調達レッグ（参照金利・フィキシング）
    "funding.floating_rate_index",            "資金調達", "資金調達の参照金利（変動金利インデックス）", "text", TRUE,
    "funding.index_tenor",                    "資金調達", "参照金利の期間",                       "code",    FALSE,
    "funding.spread_bp",                      "資金調達", "スプレッド（bp）",                     "number",  TRUE,
    "funding.rate_floor_pct",                 "資金調達", "参照金利の下限（%）",                  "number",  FALSE,
    "funding.day_count",                      "資金調達", "日数計算方式",                         "text",    TRUE,
    "funding.compounding_method",             "資金調達", "複利・平均の方法",                     "code",    TRUE,
    "funding.payment_frequency",              "資金調達", "支払の頻度",                           "code",    TRUE,
    "funding.reset_frequency",                "資金調達", "リセットの頻度",                       "code",    TRUE,
    "funding.reset_relative_to",              "資金調達", "リセット日の基準（PERIOD_START／PERIOD_END）", "code", TRUE,
    "funding.fixing_offset_days",             "資金調達", "フィキシング日（リセット日からの営業日数。前は負）", "integer", TRUE,
    "funding.fixing_calendar",                "資金調達", "フィキシングの営業日カレンダー",       "code",    TRUE,
    "funding.fixing_business_day_convention", "資金調達", "フィキシング日の営業日調整",           "code",    FALSE,
    "funding.lookback_days",                  "資金調達", "ルックバック（営業日）",               "integer", FALSE,
    "funding.observation_shift",              "資金調達", "観測期間シフトの有無（YES／NO）",      "code",    FALSE,
    "funding.payment_lag_days",               "資金調達", "支払ラグ（営業日）",                   "integer", FALSE,
    "funding.payment_business_day_convention","資金調達", "支払日の営業日調整",                   "code",    TRUE,
    "funding.payment_calendar",               "資金調達", "支払日の営業日カレンダー",             "code",    TRUE
  )
}
