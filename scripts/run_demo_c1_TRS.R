# c1 デモ：TRSターム条件の抽出JSON → 照合 → 確認表（Excel）・FpML
# CURIEプロジェクトの最上位フォルダで実行する。
#   1. 架空の記入済みPDF（2つのAIの抽出に見立てたJSON）：人の確認なし → 人の確認あり → 正式FpML
#   2. 公開サンプル（銀行のドラフト・ひな形）：data/c1_trs/ に抽出JSONがあれば、Claude単独抽出として処理

if (!file.exists("CURIE.Rproj")) stop("CURIEプロジェクトの最上位フォルダで実行してください。")
source("R/load_CURIE.R", encoding = "UTF-8")
source("scripts/demo_data/demo_trs_terms.R", encoding = "UTF-8")
suppressPackageStartupMessages(library(dplyr))

demo <- create_demo_trs_terms_c1(file.path("data", "demo_c1_trs"))
out <- file.path("output", "c1_trs")

# ---- 1a. 架空PDF：2つのAIで照合（人の確認なし） --------------------------------
before <- workflows_trs_terms_c1(demo$extractions, file.path(out, "demo_before_review"), pdf_dir = demo$pdf_dir,
                                 agents = c("claude", "copilot"), output_name = "demo_confirmation_before_review")
cat("\n== 1a. 架空PDF：人の確認前 ==\n")
print(count(before$table, status, judgement))
print(before$table |> filter(judgement == "要確認") |>
        select(field, status, value_claude, value_copilot, judgement_reason), width = Inf)
print(before$documents |> select(document_id, required_total, required_confirmed, fpml_status))

# ---- 1b. 人の確認を反映 → 正式FpML ----------------------------------------------
after <- workflows_trs_terms_c1(demo$extractions, file.path(out, "demo_after_review"), pdf_dir = demo$pdf_dir,
                                agents = c("claude", "copilot"), overrides_path = demo$overrides,
                                output_name = "demo_confirmation_after_review")
cat("\n== 1b. 架空PDF：人の確認後 ==\n")
print(after$documents |> select(document_id, required_total, required_confirmed, fpml_status))
print(after$fpml_files |> select(kind, fpml_file, well_formed))

# 同じ入力から同じFpMLができること（ハッシュの比較）
again <- workflows_trs_terms_c1(demo$extractions, file.path(out, "demo_after_review_again"), pdf_dir = demo$pdf_dir,
                                agents = c("claude", "copilot"), overrides_path = demo$overrides,
                                output_name = "demo_confirmation_after_review")
same_fpml <- identical(after$fpml_files$sha256, again$fpml_files$sha256)
cat("再実行で同じFpML（SHA-256一致）:", same_fpml, "\n")

# 引用の改ざん・取り違えを検出できること（引用を書き換えた抽出JSONで確認）
tampered <- file.path(tempdir(), "DEMO_TRS_0001__claude.json")
x <- jsonlite::fromJSON(demo$extractions[["claude"]], simplifyVector = FALSE)
x$fields <- purrr::map(x$fields, \(f) { if (f$field == "funding.day_count") f$quote <- "Day Count Fraction: Actual/360"; f })
jsonlite::write_json(x, tampered, auto_unbox = TRUE, pretty = TRUE, null = "null")
check <- workflows_trs_terms_c1(c(tampered, demo$extractions[["copilot"]]), file.path(out, "demo_tampered"),
                                pdf_dir = demo$pdf_dir, agents = c("claude", "copilot"), overrides_path = demo$overrides,
                                output_name = "demo_tampered")
tampered_row <- check$table |> filter(field == "funding.day_count")
cat("書き換えた引用の判定:", tampered_row$judgement, "／", tampered_row$quote_check, "\n")

# ---- 2. 公開サンプル（あれば） ---------------------------------------------------
public_ext <- list.files(file.path("data", "c1_trs", "extractions", "public"), "[.]json$", full.names = TRUE)
public <- NULL
if (length(public_ext)) {
  public <- workflows_trs_terms_c1(public_ext, file.path(out, "public"), pdf_dir = file.path("data", "c1_trs", "public_samples"),
                                   agents = "claude", output_name = "public_samples_confirmation")
  cat("\n== 2. 公開サンプル（Claude単独抽出） ==\n")
  print(public$documents |> select(document_id, required_total, required_confirmed, fpml_status))
  print(count(public$table, document_id, status))
  print(count(public$table, quote_check))
}

stopifnot(
  !before$documents$fpml_final,
  all(c("funding.spread_bp", "funding.lookback_days") %in% before$table$field[before$table$judgement == "要確認"]),
  after$documents$fpml_final,
  all(after$fpml_files$well_formed),
  same_fpml,
  tampered_row$judgement == "要確認",
  is.null(public) || (all(!public$documents$fpml_final) && all(public$table$quote_check %in% c("一致", "引用なし")))
)
cat("\n確認：すべて合格\n")
