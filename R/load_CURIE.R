# CURIE本体の読込（デモデータの生成は含まない。デモは scripts/ を参照）。
# CURIEプロジェクトの最上位フォルダで source("R/load_CURIE.R", encoding = "UTF-8") とする。
#
# R/
#   Z0_utility/       共通関数（Part IIから移植した関数、取込・変換・検証・属性取得・カーブ・債券計算）
#   A1_offer_sheet/   Phase 1：オファーシート作成（デモ実装済み・本番連携は未完了）
#   A2_valuation/     Phase 1：時価適正性の検証（未実装）
#   A3_credit_reports/ Phase 1：クレジットレポート読込・紐づけ（未実装）
#   A4_daily_report/  Phase 1：日次レポート・目玉オファー（未実装）
#   B1_fund_strategy/ Phase 2：ファンド戦略・制約条件の読込（未実装）
#   B2_rebalancing/   Phase 2：ポートフォリオ入れ替え提案（未実装）
#   B3_aladdin_orders/ Phase 2：Aladdin APIによる発注（未実装）

source(file.path("R", "Z0_utility", "00_dependencies.R"), encoding = "UTF-8")
check_packages_CURIE()
load_lagrange_functions_CURIE()   # 移植したPart IIの関数（R/Z0_utility/lagrange/）

.curie_modules <- list(
  Z0_utility = c("isin.R", "parse_utils.R", "import_settings.R", "market_data_import.R",
                 "interpolation.R", "bond_yield_YEN.R", "attributes.R", "bond_build.R",
                 "curve_build.R", "environment_info.R"),
  A1_offer_sheet = c("convert_offer_sheet.R", "spread_inputs.R", "validate_offer_worksheet.R",
                     "compute_offer_pricing.R", "write_offer_results_excel.R", "workflows_offer_sheet.R")
)
purrr::iwalk(.curie_modules, \(files, dir) purrr::walk(files, \(f) source(file.path("R", dir, f), encoding = "UTF-8")))
