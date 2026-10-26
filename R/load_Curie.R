# Curie本体の読込（デモデータの生成は含まない。デモは scripts/ を参照）。
# Curieプロジェクトの最上位フォルダで source("R/load_Curie.R", encoding = "UTF-8") とする。
#
# R/
#   z0_utility/       共通関数（Part IIから移植した関数、取込・変換・検証・属性取得・カーブ・債券計算）
#   a1_offer_sheet/   Phase 1：オファーシート作成（デモ実装済み・本番連携は未完了）
#   a2_valuation/     Phase 1：時価適正性の検証（未実装）
#   a3_credit_reports/ Phase 1：クレジットレポート読込・紐づけ（未実装）
#   a4_daily_report/  Phase 1：日次レポート・目玉オファー（未実装）
#   b1_fund_strategy/ Phase 2：ファンド戦略・制約条件の読込（未実装）
#   b2_rebalancing/   Phase 2：ポートフォリオ入れ替え提案（未実装）
#   b3_aladdin_orders/ Phase 2：Aladdin APIによる発注（未実装）

# 個人用の設定（例：Curie_HTTP_USER_AGENT）。tmp/.Renviron があれば読み込む（Git に入れない。値は表示しない）
if (file.exists(file.path("tmp", ".Renviron"))) readRenviron(file.path("tmp", ".Renviron"))
# Curie のフォルダー（作業ディレクトリに依存しない処理が使う。未設定なら読み込んだ場所）
if (!nzchar(Sys.getenv("Curie_ROOT"))) Sys.setenv(Curie_ROOT = normalizePath(getwd(), winslash = "/"))

source(file.path("R", "z0_utility", "00_dependencies.R"), encoding = "UTF-8")
check_packages_Curie()
load_lagrange_functions_Curie()   # 移植したPart IIの関数（R/z0_utility/lagrange/）

.curie_modules <- list(
  z0_utility = c("console.R", "isin.R", "parse_utils.R", "import_settings.R", "market_data_import.R",
                 "interpolation.R", "bond_yield_YEN.R", "attributes.R", "bond_build.R",
                 "curve_build.R", "environment_info.R", "report_format.R", "excel_format.R"),
  a1_offer_sheet = c("convert_offer_sheet.R", "spread_inputs.R", "validate_offer_worksheet.R",
                     "compute_offer_pricing.R", "write_offer_results_excel.R", "workflows_offer_sheet.R",
                     "report_tables.R"),
  # c1 は jsonlite・xml2・pdftools・openssl・curl・processx を使う（関数を呼んだときに必要）
  c1_trs_terms = c("trs_fields.R", "verify_quotes.R", "fpml_trs.R", "fpml_validate.R", "acquire_document.R",
                   "reading_data.R", "claude_agent.R", "reader.R", "read_results.R", "validate_results.R", "aladdin_payload.R",
                   "workflows_c1_case.R", "inbox_c1.R")
)
purrr::iwalk(.curie_modules, \(files, dir) purrr::walk(files, \(f) source(file.path("R", dir, f), encoding = "UTF-8")))
