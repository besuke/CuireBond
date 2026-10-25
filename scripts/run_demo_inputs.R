# CURIE デモ（入力まで）：データ生成 → オファー変換 → 属性結合 → 入力検証
# ASW計算とExcel出力は行わない（scripts/run_demo_JPY.R・run_demo_ISO.R を参照）。
# CURIEプロジェクトの最上位フォルダで実行する。データはすべて架空のテスト用（ISINも架空、市場価格も実在しない）。

if (!file.exists("CURIE.Rproj")) stop("CURIEプロジェクトの最上位フォルダで実行してください。")
source("R/load_CURIE.R", encoding = "UTF-8")
source("scripts/demo_data/prepare_demo_inputs.R", encoding = "UTF-8")
suppressPackageStartupMessages(library(dplyr))

# 1. データ生成（オファーExcel・銘柄属性・市場データ。市場日は統一）
demo <- prepare_demo_inputs_CURIE()
vd <- demo$marketdate
work_root <- file.path("output", "work", "inputs_demo")
cat("市場日：", format(vd), "\n", sep = "")

# 2. オファー変換：見出しから対象シートを判定する。候補が複数なら明示指定が必要
cat("\n■ 見出し行の候補\n")
candidates <- purrr::map(c(JPY = demo$offer_jpy, ISO = demo$offer_iso), detect_offer_header_CURIE) |>
  purrr::list_rbind(names_to = "ブック")
print(select(candidates, ブック, any_of(c("sheet", "header_row"))), n = Inf)
ambiguous <- tryCatch(convert_offer_sheet_CURIE(demo$offer_jpy, currency = "JPY"), error = function(e) e)
if (inherits(ambiguous, "error")) cat("\nJPY でシートを指定しない場合：", conditionMessage(ambiguous), "\n", sep = "")

# 3. 属性結合 → 4. 入力検証（ワークフローと同じ関数を使う）
run_inputs <- function(workflow, offer_path, offer_settings, ois_paths, market) {
  work_dir <- file.path(work_root, workflow)
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
  prepared <- .prepare_offers_CURIE(workflow, offer_path, offer_settings, demo$attribute_fetcher, vd, work_dir)
  ois <- import_ois_files_CURIE(ois_paths, text_import_settings_CURIE(), work_dir)
  worksheet <- validate_offer_worksheet_CURIE(workflow, prepared$offers, prepared$standard_file,
                                              prepared$attribute_snapshot, ois$normalized_paths, market, vd)
  list(prepared = prepared, ois = ois, worksheet = worksheet)
}
jpy <- run_inputs("JPY", demo$offer_jpy, offer_import_settings_CURIE(sheet = "円債オファー", currency = "JPY"),
                  demo$ois_jpy, list(jgb = import_jgb_file_CURIE(demo$jgb, text_import_settings_CURIE())))
iso <- run_inputs("ISO", demo$offer_iso, offer_import_settings_CURIE(),
                  c(demo$ois_usd, demo$ois_eur, demo$ois_jpy),
                  list(fx = import_fx_spot_CURIE(demo$fx, text_import_settings_CURIE()),
                       basis = import_ccs_basis_CURIE(demo$basis, text_import_settings_CURIE())))

# 結果の表示
results <- list(JPY = jpy, ISO = iso)
cat("\n■ 取り込み件数\n")
print(purrr::imap(results, \(x, w) tibble::tibble(
  ワークフロー = w, オファー行 = nrow(x$prepared$offers), 属性あり = sum(!is.na(x$worksheet$attribute_date)),
  検証合格 = sum(x$worksheet$validation_status == "valid"), 検証不合格 = sum(x$worksheet$validation_status != "valid"),
  OIS気配 = nrow(x$ois$quotes))) |> purrr::list_rbind())
purrr::iwalk(results, function(x, w) {
  cat("\n■ ", w, "：検証結果\n", sep = "")
  print(x$worksheet |>
          select(any_of(c("source_file", "source_sheet", "source_row", "ISIN", "currency", "offer_price", "issue_price",
                          "offer_size", "offer_settlementdate", "broker_names", "validation_status", "validation_message"))),
        n = Inf, width = Inf)
})
