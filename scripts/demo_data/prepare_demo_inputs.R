# デモの入力ファイルをまとめて作り、パスとデモ属性取得関数を返す（本体のワークフローとは分離）。
# Curie本体（R/load_Curie.R）を読んだ後に source() する。

# 読込先はグローバル環境（local = globalenv() で明示）。順序は common → offers → master → market
purrr::walk(
  c("demo_data_common.R", "demo_data_offers.R", "demo_data_master.R", "demo_data_market.R"),
  \(f) source(file.path("scripts", "demo_data", f), encoding = "UTF-8", local = globalenv())
)

prepare_demo_inputs_Curie <- function(dir = file.path("data", "demo"), marketdate = demo_marketdate_Curie) {
  offers <- create_demo_offers_Curie(file.path(dir, "offers"), marketdate)
  attributes <- create_demo_attributes_Curie(file.path(dir, "attributes"), marketdate)
  market <- create_demo_market_data_Curie(file.path(dir, "market"), marketdate)
  list(
    marketdate = marketdate,
    offer_jpy = offers[["JPY"]], offer_iso = offers[["ISO"]],
    ois_jpy = market[["ois_JPY"]], ois_usd = market[["ois_USD"]], ois_eur = market[["ois_EUR"]],
    jgb = market[["jgb"]], fx = market[["fx"]], basis = market[["basis"]],
    attribute_fetcher = make_demo_attribute_fetcher_Curie(attributes)
  )
}
