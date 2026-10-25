# デモデータ共通の定義と補助関数（本体のワークフローとは分離）。
# ここで定義する銘柄・価格・市場気配はすべて架空のテスト用データであり、
# 実在の銘柄・発行体・市場価格とは関係がない。

demo_marketdate_CURIE <- as.Date("2026-10-06")

make_test_isin_CURIE <- function(body) paste0(toupper(body), isin_check_digit_CURIE(body))

# 債券生成の規約（通貨別）。LagrangeFinance のサンプル（sample_bond_asset_swap.R）と同じ値。
# day_count は GaussQuant::day_counter_GQL() の名前（ActualActual_Bond は ISMA と同じ規約）。
# calendar は R/compute_offer_pricing.R の calendar_from_name_CURIE() の名前。
demo_bond_conventions_CURIE <- function() {
  tibble::tribble(
    ~currency, ~day_count,            ~calendar,                     ~settlement_days,
    "JPY",     "Actual365Fixed",      "Japan",                       2L,
    "USD",     "Thirty360_BondBasis", "UnitedStates_GovernmentBond", 2L,
    "EUR",     "ActualActual_Bond",   "TARGET",                      2L
  ) |>
    dplyr::mutate(
      payment_convention = "Following", schedule_convention = "Unadjusted",
      maturity_convention = "Unadjusted", date_generation = "Backward", end_of_month = FALSE
    )
}

# 架空銘柄の一覧（銘柄属性とオファーの両方の元になる）。
# ISINは「TEST」を含む架空コードで、チェックディジットだけは規則どおりに計算する。
# - JPY側：円債4銘柄＋ユーロ円債1銘柄（建て通貨JPY）＋変動利付1銘柄（未対応の確認用）
# - ISO側：USD 3銘柄・EUR 2銘柄＋コーラブル1銘柄（未対応の確認用）
demo_bonds_CURIE <- function() {
  tibble::tribble(
    ~isin_body,    ~security_name,                    ~currency, ~coupon_type, ~coupon_rate, ~coupon_frequency, ~issue_date,  ~issue_price, ~maturity_date, ~floating_index, ~floating_spread_bp, ~reset_frequency, ~first_call_date, ~call_price,
    "JPTEST00001", "テスト円債A 0.50% 2028/9",        "JPY",     "FIXED",      0.0050,       2L,                "2023-09-20", 99.95,        "2028-09-20",   NA,              NA,                  NA,               NA,               NA,
    "JPTEST00002", "テスト円債B 0.90% 2031/3",        "JPY",     "FIXED",      0.0090,       2L,                "2024-03-15", 100.00,       "2031-03-15",   NA,              NA,                  NA,               NA,               NA,
    "JPTEST00003", "テスト円債C 1.60% 2035/6",        "JPY",     "FIXED",      0.0160,       2L,                "2025-06-20", 99.80,        "2035-06-20",   NA,              NA,                  NA,               NA,               NA,
    "JPTEST00004", "テスト円債D 0.35% 2029/12",       "JPY",     "FIXED",      0.0035,       2L,                "2022-12-20", 100.00,       "2029-12-20",   NA,              NA,                  NA,               NA,               NA,
    "XSTESTEY001", "テスト ユーロ円債E 1.75% 2033/4", "JPY",     "FIXED",      0.0175,       2L,                "2026-04-24", 100.00,       "2033-04-24",   NA,              NA,                  NA,               NA,               NA,
    "JPTESTFRN01", "テスト円変動利付債 TONA+30 2030", "JPY",     "FLOATING",   0.0105,       4L,                "2025-03-14", 100.00,       "2030-03-14",   "TONAR",         30,                  "Quarterly",      NA,               NA,
    "USTEST00001", "TEST USD BOND F 4.25% 2029",      "USD",     "FIXED",      0.0425,       2L,                "2024-02-15", 99.70,        "2029-02-15",   NA,              NA,                  NA,               NA,               NA,
    "USTEST00002", "TEST USD BOND G 4.60% 2032",      "USD",     "FIXED",      0.0460,       2L,                "2025-05-01", 99.85,        "2032-05-01",   NA,              NA,                  NA,               NA,               NA,
    "USTEST00003", "TEST USD BOND H 5.10% 2033",      "USD",     "FIXED",      0.0510,       2L,                "2023-11-10", 100.00,       "2033-11-10",   NA,              NA,                  NA,               NA,               NA,
    "XSTEST00001", "TEST EUR BOND I 3.00% 2030",      "EUR",     "FIXED",      0.0300,       1L,                "2024-06-12", 99.60,        "2030-06-12",   NA,              NA,                  NA,               NA,               NA,
    "XSTEST00002", "TEST EUR BOND J 3.25% 2034",      "EUR",     "FIXED",      0.0325,       1L,                "2025-01-22", 99.90,        "2034-01-22",   NA,              NA,                  NA,               NA,               NA,
    "USTESTCAL01", "TEST USD CALLABLE K 5.50% 2036",  "USD",     "FIXED",      0.0550,       2L,                "2026-03-01", 100.00,       "2036-03-01",   NA,              NA,                  NA,               "2031-03-01",     100
  ) |>
    dplyr::mutate(
      ISIN = make_test_isin_CURIE(.data$isin_body),
      dplyr::across(c("issue_date", "maturity_date", "first_call_date"), as.Date),
      floating_index = as.character(.data$floating_index),
      reset_frequency = as.character(.data$reset_frequency),
      floating_spread_bp = as.numeric(.data$floating_spread_bp),
      call_price = as.numeric(.data$call_price),
      .before = 1
    ) |>
    dplyr::select(-"isin_body") |>
    dplyr::left_join(demo_bond_conventions_CURIE(), by = "currency")
}

# 架空のJGB（比較用国債）。単価は R/demo/demo_data_market.R で生成する。
demo_jgbs_CURIE <- function() {
  tibble::tribble(
    ~isin_body,    ~security_name,           ~coupon_rate, ~issue_date,  ~maturity_date,
    "JPTESTJGB01", "テスト国債 2年 0.80%",   0.0080,       "2025-10-01", "2027-10-01",
    "JPTESTJGB02", "テスト国債 5年 1.00%",   0.0100,       "2023-12-20", "2028-12-20",
    "JPTESTJGB03", "テスト国債 5年 1.10%",   0.0110,       "2025-06-20", "2030-06-20",
    "JPTESTJGB04", "テスト国債 10年 0.70%",  0.0070,       "2021-12-20", "2031-12-20",
    "JPTESTJGB05", "テスト国債 10年 1.20%",  0.0120,       "2023-06-20", "2033-06-20",
    "JPTESTJGB06", "テスト国債 10年 1.50%",  0.0150,       "2025-09-20", "2035-09-20",
    "JPTESTJGB07", "テスト国債 20年 2.10%",  0.0210,       "2026-06-20", "2046-06-20",
    # JGB05 と同じ償還日の別銘柄（同一償還日の利回りを平均する処理の確認用）
    "JPTESTJGB08", "テスト国債 20年 0.90%",  0.0090,       "2013-06-20", "2033-06-20"
  ) |>
    dplyr::mutate(
      ISIN = make_test_isin_CURIE(.data$isin_body), currency = "JPY", coupon_frequency = 2L,
      issue_date = as.Date(.data$issue_date), maturity_date = as.Date(.data$maturity_date),
      redemption = 100, .before = 1
    ) |>
    dplyr::select(-"isin_body")
}

# 文字の表（1行目から順にExcelの行になる）をそのまま書き出すための変換。
# writexl は列ごとに型を1つしか持てないため、見出しより上の説明行を置くシートは
# すべて文字列セルで書く。数値・日付の正規化は変換関数側の責務とする。
rows_to_sheet_CURIE <- function(rows) {
  width <- max(lengths(rows))
  rows |>
    purrr::map(function(r) c(r, rep(NA_character_, width - length(r)))) |>
    purrr::map(function(r) rlang::set_names(as.list(r), paste0("V", seq_len(width)))) |>
    dplyr::bind_rows()
}

# 説明用シート（1行目が「項目・内容」の見出し、2行目以降が説明）
description_sheet_CURIE <- function(items) {
  rows_to_sheet_CURIE(c(list(c("項目", "内容")), purrr::imap(items, function(v, k) c(k, v))))
}
