# CURIE本体の読込（デモデータ生成は含まない。デモは R/demo/ を別に読む）。
# CURIEプロジェクトの最上位フォルダで source("R/load_CURIE.R", encoding = "UTF-8") とする。

source(file.path("R", "00_dependencies.R"), encoding = "UTF-8")
check_packages_CURIE()
load_lagrange_functions_CURIE()   # 移植したPart IIの関数（R/lagrange/）
for (.f in c("isin.R", "convert_offer_sheet.R", "import_settings.R", "attributes.R", "market_data.R",
             "validate_offer_worksheet.R", "compute_offer_pricing.R",
             "write_offer_results_excel.R", "environment_info.R", "workflows_offer_sheet.R")) {
  source(file.path("R", .f), encoding = "UTF-8")
}
rm(.f)
