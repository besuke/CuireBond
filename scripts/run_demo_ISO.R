# Curie デモ（ISOのみ）：外貨オファー → 自国OIS ASW・円換算ASW → Excel出力
# Curieプロジェクトの最上位フォルダで実行する。データはすべて架空のテスト用。

if (!file.exists("Curie.Rproj")) stop("Curieプロジェクトの最上位フォルダで実行してください。")
source("R/load_Curie.R", encoding = "UTF-8")
source("scripts/demo_data/prepare_demo_inputs.R", encoding = "UTF-8")

demo <- prepare_demo_inputs_Curie()

iso <- workflows_offer_sheet_ISO(
  offer_path = demo$offer_iso,
  ois_paths = c(demo$ois_usd, demo$ois_eur, demo$ois_jpy),   # 円換算ASWにはJPYのOISも使う
  basis_path = demo$basis,
  fx_path = demo$fx,
  valuation_date = demo$marketdate,
  output_path = file.path("output", "offer_results_ISO_demo_20261006.xlsx"),
  attribute_fetcher = demo$attribute_fetcher,
  work_dir = file.path("output", "work", "ISO")
)

print(dplyr::count(iso$results, currency, validation_status, pricing_status))
print(iso$results |>
        dplyr::select(ISIN, currency, offer_price, native_asw_bp, basis_bp, jpy_asw_bp, pricing_status),
      width = Inf)
cat("出力: ", iso$output_path, "\n", sep = "")
