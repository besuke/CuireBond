# Curie デモ（JPYのみ）：円オファー → JPY OIS ASW・JGBスプレッド → Excel出力
# Curieプロジェクトの最上位フォルダで実行する。データはすべて架空のテスト用。

if (!file.exists("Curie.Rproj")) stop("Run this script from the top folder of the Curie project.")
source("R/load_Curie.R", encoding = "UTF-8")
source("scripts/demo_data/prepare_demo_inputs.R", encoding = "UTF-8")

demo <- prepare_demo_inputs_Curie()

jpy <- workflows_offer_sheet_JPY(
  offer_path = demo$offer_jpy,
  ois_path = demo$ois_jpy,
  jgb_path = demo$jgb,
  valuation_date = demo$marketdate,
  output_path = file.path("output", "offer_results_JPY_demo_20261006.xlsx"),
  attribute_fetcher = demo$attribute_fetcher,
  offer_settings = offer_import_settings_Curie(sheet = "円債オファー", currency = "JPY"),
  work_dir = file.path("output", "work", "JPY")
)

print(dplyr::count(jpy$results, validation_status, pricing_status))
print(jpy$results |>
        dplyr::select(ISIN, security_name, offer_price, native_asw_bp, jgb_spread_bp, pricing_status),
      width = Inf)
say_path_Curie("Excel output saved.", jpy$output_path, "file")
