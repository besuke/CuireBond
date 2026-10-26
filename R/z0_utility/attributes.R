# 銘柄属性の取得（ISINをキーとする）。
#
# ワークフローには「属性取得関数」を引数で渡す。
#   attribute_fetcher(ISIN, valuation_date) -> 1銘柄1行の tibble（下の列）
# 本番ではAladdin APIから取得する関数に差し替える。APIのエンドポイント・項目名・
# 認証はここでは扱わない（仕様の提示後に、API項目からこの列名への対応を実装する）。
#
# 列名は LagrangeFinance の銘柄属性の列名に合わせる。maturity_date は最終償還日。
# First Call日（first_call_date）は最終償還日とは別に持ち、計算終期は別の列で明示する。

.attribute_columns_required_Curie <- c(
  "ISIN", "attribute_date", "currency",
  "coupon_type",                     # FIXED / FLOATING
  "coupon_rate",                     # 小数。変動利付は取得時点のクーポン（参考）
  "issue_date", "issue_price",       # 発行日、発行単価（額面100当たり）
  "maturity_date",                   # 最終償還日
  "redemption",                      # 償還額（額面100当たり）
  "coupon_frequency", "day_count",   # 利払回数（年）、日数計算方式（GaussQuant::day_counter_GQL の名前）
  "settlement_days", "calendar",     # 受渡日数、営業日カレンダー
  "payment_convention", "schedule_convention", "maturity_convention", "date_generation"
)

.attribute_columns_optional_Curie <- c(
  "security_name", "issuer_name", "redemption_type", "end_of_month",
  # 変動利付
  "floating_index", "floating_spread_bp", "reset_frequency", "reset_lag_days",
  # 期限前償還
  "first_call_date", "call_price", "call_type",
  "attribute_source", "attribute_retrieved_at"
)

# 取得結果の形を確かめ、任意列がなければ NA で追加する
check_attribute_schema_Curie <- function(attributes) {
  if (!is.data.frame(attributes)) stop("属性取得関数は data.frame / tibble を返してください。", call. = FALSE)
  missing <- setdiff(.attribute_columns_required_Curie, names(attributes))
  if (length(missing)) {
    stop("属性取得関数の結果に必須列がありません: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  absent <- setdiff(.attribute_columns_optional_Curie, names(attributes))
  attributes[absent] <- NA
  attributes |>
    tibble::as_tibble() |>
    dplyr::mutate(
      ISIN = stringr::str_to_upper(stringr::str_trim(.data$ISIN)),
      coupon_type = stringr::str_to_upper(.data$coupon_type),
      dplyr::across(c("attribute_date", "issue_date", "maturity_date", "first_call_date"), as.Date)
    )
}

# 計算終期。現在の計算関数（固定利付・満期一括・償還額100）が対応する銘柄だけ最終償還日とし、
# それ以外は計算終期を決めず、未対応の理由を返す（固定利付債として代用しない）。
calculation_end_Curie <- function(attributes) {
  attributes |>
    dplyr::mutate(
      unsupported_reason = dplyr::case_when(
        is.na(.data$coupon_type) ~ NA_character_,
        .data$coupon_type != "FIXED" ~ paste0("変動利付等（coupon_type = ", .data$coupon_type,
                                             "）は現在の計算関数の対象外"),
        !is.na(.data$first_call_date) ~ paste0("期限前償還条項あり（First Call日 ", .data$first_call_date,
                                              "）。現在の計算関数はオプション価値を扱わない"),
        !is.na(.data$redemption_type) & .data$redemption_type != "BULLET" ~
          paste0("満期一括償還以外（", .data$redemption_type, "）は対象外"),
        abs(.data$redemption - 100) > 1e-12 ~ "償還額が100以外は対象外",
        .default = NA_character_
      ),
      calculation_end_date = dplyr::if_else(is.na(.data$unsupported_reason), .data$maturity_date, as.Date(NA)),
      calculation_end_basis = dplyr::if_else(
        is.na(.data$unsupported_reason), "最終償還日（固定利付・満期一括償還・償還額100）",
        paste("未決定：", .data$unsupported_reason))
    )
}

# 属性取得関数を呼び、形を確かめ、既存関数（import_bond_master_attribute_ADP）へ渡すRDSを書く。
# このRDSは「計算に使った属性」の記録にもなる。
fetch_attributes_Curie <- function(attribute_fetcher, isin, valuation_date, snapshot_path) {
  if (!is.function(attribute_fetcher)) stop("attribute_fetcher には関数を渡してください。", call. = FALSE)
  attributes <- attribute_fetcher(unique(isin), valuation_date) |>
    check_attribute_schema_Curie() |>
    calculation_end_Curie()
  if (anyDuplicated(attributes[c("ISIN", "attribute_date")])) {
    stop("属性取得関数の結果で ISIN + attribute_date が重複しています。", call. = FALSE)
  }
  dir.create(dirname(snapshot_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(attributes, snapshot_path)
  list(attributes = attributes, snapshot_path = normalizePath(snapshot_path, winslash = "/"))
}
