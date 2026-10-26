# c1：TRS 確認書1件の処理（R → Claude Code → R）。コンソールには取引の要約と payload の保存先を表示する
# Curieプロジェクトの最上位フォルダで実行する。
#
#   source("scripts/run_c1_document.R", encoding = "UTF-8")      # RStudio：資料のURLかファイルのパスを貼る
#   c1_args <- c("--claude=demo", "--claude-demo-dir=<フォルダ>"); source(...)   # RStudio でモードを指定する
#   Rscript scripts/run_c1_document.R <URLまたはファイルのパス> [--claude=api|demo] [--claude-demo-dir=<フォルダ>]
#
# 入力：公開 PDF・HTML の URL、ローカルの PDF・HTML（Box Drive の同期フォルダのファイルも可。形式は自動で判定する）。
# 成果物：result/conditions.json と FpML（検証済みなら fpml_final.xml、それ以外は fpml_DRAFT.xml）、
#         Aladdin の payload 下書き（aladdin_request_DRAFT.json・aladdin_unresolved.json。送信はしない）。
# Pay／Receive は自社から見た向き。自社の名前は対応表（Curie_ALADDIN_MAP）の our_party_names で指定する（なければ Unknown）。

if (!file.exists("Curie.Rproj")) stop("Run this script from the top folder of the Curie project.")
source("R/load_Curie.R", encoding = "UTF-8")

# RStudio で source() するときは、事前に c1_args <- c("--claude=demo", ...) と書けば同じ指定ができる
args <- if (exists("c1_args", envir = globalenv())) get("c1_args", envir = globalenv()) else commandArgs(trailingOnly = TRUE)
opt <- function(name, default = NULL) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit)) sub(paste0("^--", name, "="), "", hit[1]) else default
}
source_arg <- if (length(args) && !startsWith(args[1], "--")) args[1] else NA_character_
if (is.na(source_arg)) {
  prompt <- "Paste the document URL (or file path): "
  source_arg <- if (interactive()) readline(prompt) else { cat(prompt); readLines(file("stdin"), n = 1L, warn = FALSE) }
  source_arg <- trimws(gsub('^"|"$', "", source_arg))
}
if (!length(source_arg) || !nzchar(source_arg)) stop("No URL or file path was entered.", call. = FALSE)

case <- workflows_c1_case(source_arg, opt("claude", "api"), claude_demo_dir = opt("claude-demo-dir"))

# コンソールには取引の要約と payload の保存先だけを出す。抽出条件・検証・未確定項目の詳細は result/ のファイルに残す
show <- function(x) cat(paste0(sprintf("%-13s", paste0(names(x), ":")), x), sep = "\n")
cat("\n")
if (!is.null(case$results$failure)) {
  f <- case$results$failure
  say_path_Curie(paste0("Processing stopped. ", .failure_kind_en_c1(f$kind), ". ", f$message), case$case_dir, "dir")
}
if (!is.null(case$validation$conditions)) show(deal_summary_c1(case$validation$conditions))
if (!is.null(case$aladdin)) {
  cat("\n")
  say_path_Curie("Payload draft saved. NOT SENT.", case$aladdin$path, "file")
}
