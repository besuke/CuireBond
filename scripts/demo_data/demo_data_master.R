# デモの銘柄属性と、デモ用の属性取得関数（本番では Aladdin API を使う関数に差し替える）。
# 属性取得関数の約束は R/Z0_utility/attributes.R：function(ISIN, valuation_date) -> 1銘柄1行の tibble。
# ISINと銘柄はすべて架空のテスト用で、実在しない。

create_demo_attributes_CURIE <- function(dir, marketdate = demo_marketdate_CURIE) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  attributes <- demo_bonds_CURIE() |>
    dplyr::mutate(
      attribute_date = marketdate,
      issuer_name = "架空発行体（テスト用）",
      redemption = 100,
      redemption_type = "BULLET",
      reset_lag_days = NA_integer_,
      call_type = dplyr::if_else(is.na(.data$first_call_date), NA_character_, "issuer_call"),
      attribute_source = "DEMO（架空のテスト用属性。Aladdin APIではない）",
      attribute_retrieved_at = paste0(format(marketdate), "T08:00:00+09:00")
    )
  path <- file.path(dir, paste0("demo_attributes_", format(marketdate, "%Y%m%d"), ".rds"))
  saveRDS(attributes, path)
  invisible(path)
}

# デモの属性取得関数を作る。RDSから、指定ISIN・評価日と同じ属性日の行を返す。
# 見つからないISINは返さない（ワークフロー側で missing_bond_attributes になる）。
make_demo_attribute_fetcher_CURIE <- function(path) {
  force(path)
  function(isin, valuation_date) {
    readRDS(path) |>
      dplyr::filter(.data$ISIN %in% isin, .data$attribute_date == as.Date(valuation_date))
  }
}
