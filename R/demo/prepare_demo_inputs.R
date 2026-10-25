# デモの入力ファイルをまとめて作り、パスとデモ属性取得関数を返す（本体のワークフローとは分離）。
# CURIE本体（R/load_CURIE.R）を読んだ後に source() する。

for (.f in c("demo_data_common.R", "demo_data_offers.R", "demo_data_master.R", "demo_data_market.R")) {
  source(file.path("R", "demo", .f), encoding = "UTF-8")
}
rm(.f)

prepare_demo_inputs_CURIE <- function(dir = file.path("data", "demo"), marketdate = demo_marketdate_CURIE) {
  offers <- create_demo_offers_CURIE(file.path(dir, "offers"), marketdate)
  attributes <- create_demo_attributes_CURIE(file.path(dir, "attributes"), marketdate)
  market <- create_demo_market_data_CURIE(file.path(dir, "market"), marketdate)
  list(
    marketdate = marketdate,
    offer_jpy = offers[["JPY"]], offer_iso = offers[["ISO"]],
    ois_jpy = market[["ois_JPY"]], ois_usd = market[["ois_USD"]], ois_eur = market[["ois_EUR"]],
    jgb = market[["jgb"]], fx = market[["fx"]], basis = market[["basis"]],
    attribute_fetcher = make_demo_attribute_fetcher_CURIE(attributes)
  )
}
