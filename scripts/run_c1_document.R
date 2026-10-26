# c1：配置済み資料1件の処理（Copilot の一次読解 → Claude の原文照合・FpML 作成 → R の検証）
# CURIEプロジェクトの最上位フォルダで実行する。
#
#   source("scripts/run_c1_document.R", encoding = "UTF-8")      # RStudio：共有URLを貼る（Copilot・Claude とも api）
#   c1_args <- c("--copilot=demo", "--copilot-file=<回答ファイル>"); source(...)   # RStudio でモードを指定する
#   Rscript scripts/run_c1_document.R <共有URL> [--copilot=api|manual|demo] [--claude=api|demo]
#           [--copilot-file=<回答ファイル>] [--claude-demo-dir=<フォルダ>] [--show-fpml]
#
# 入力：公開HTML・公開PDFのURL、ローカルのHTML・PDF、SharePoint／OneDrive の資料URL（形式は自動で判定する）。
# R・PowerShell は資料をアップロードしない。取得した同じ原資料を Copilot と Claude の両方に渡す。
# これは独立した二重読解ではない（Copilot の一次読解を Claude が原文で確かめる）。
# Copilot の api は Microsoft 365 Copilot Chat API（beta／プレビュー）を使う。接続確認は PS/test_connection.ps1。

if (!file.exists("CURIE.Rproj")) stop("CURIEプロジェクトの最上位フォルダで実行してください。")
source("R/load_CURIE.R", encoding = "UTF-8")

# RStudio で source() するときは、事前に c1_args <- c("--copilot=demo", ...) と書けば同じ指定ができる
args <- if (exists("c1_args", envir = globalenv())) get("c1_args", envir = globalenv()) else commandArgs(trailingOnly = TRUE)
opt <- function(name, default = NULL) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit)) sub(paste0("^--", name, "="), "", hit[1]) else default
}
shared_url <- if (length(args) && !startsWith(args[1], "--")) args[1] else NA_character_
if (is.na(shared_url)) {
  prompt <- "配置済み資料の共有URLを貼ってください: "
  shared_url <- if (interactive()) readline(prompt) else { cat(prompt); readLines(file("stdin"), n = 1L, warn = FALSE) }
  shared_url <- trimws(gsub('^"|"$', "", shared_url))
}
if (!length(shared_url) || !nzchar(shared_url)) stop("共有URLが入力されていません。", call. = FALSE)

copilot_mode <- opt("copilot", "api")
claude_mode <- opt("claude", "api")
cat("\nCopilot：", copilot_mode, if (copilot_mode == "api") "（Microsoft 365 Copilot Chat API。beta／プレビュー）",
    "　Claude：", claude_mode, "\n", sep = "")

case <- workflows_c1_case(shared_url, copilot_mode, claude_mode,
                          copilot_file = opt("copilot-file"), claude_demo_dir = opt("claude-demo-dir"))

cat("\n読み方：", case$reading %||% case$results$manifest$reading %||% "—", "\n状態：", case$state,
    "\n案件フォルダ：", case$case_dir, "\n", sep = "")
if (!is.null(case$results$manifest$stages)) {
  st <- case$results$manifest$stages
  cat("段階：資料 ", st$document, "／読解用データ ", st$reading_data %||% "—", "／Copilot ", st$copilot,
      "／Claude ", st$claude, "／成果物 ", st$outputs, "\n", sep = "")
  acq <- case$results$manifest$acquisition
  if (!is.null(acq)) {
    cat("取得：", acq$source_type, "、", acq$format$kind, "、HTTP ", acq$http_status %||% "—", "、", acq$retrieved_at,
        "、SHA-256 ", substr(acq$sha256, 1, 12), "…\n", sep = "")
  }
  passed <- case$results$manifest$copilot$document_passed_as
  if (!is.null(passed)) cat("Copilot への資料の渡し方：", passed, "\n", sep = "")
}
if (!is.null(case$error)) cat("エラー：", case$error, "\n")
if (!is.null(case$excel) && !is.na(case$excel)) cat("確認用Excel：", case$excel, "\n", sep = "")
if (!is.null(case$fpml) && !is.na(case$fpml)) {
  cat("FpML：", case$fpml, if (grepl("DRAFT", case$fpml)) "（ドラフト：未検証または条件不足）" else "（最終版）", "\n", sep = "")
}

if (!is.null(case$validation)) {
  checks <- case$validation$checks
  cat("\n■ 検証結果（合格 ", sum(checks$結果 == "合格"), "／不合格 ", sum(checks$結果 == "不合格"),
      "／その他 ", sum(!checks$結果 %in% c("合格", "不合格")), "）\n", sep = "")
  print(checks, n = Inf, width = Inf)
}

terms <- summarize_c1_case(case)
if (!is.null(terms)) {
  trs <- dplyr::filter(terms, 項目 == trs_field_dictionary_c1()$label_ja[1])
  cat("\n■ トータル・リターン・スワップか：",
      if (nrow(trs) && identical(trs$値[1], "TOTAL_RETURN_SWAP")) "はい" else if (nrow(trs) && nzchar(trs$値[1])) "いいえ" else "判定できない",
      if (nrow(trs) && nzchar(trs$ページ[1])) paste0("（根拠 p.", trs$ページ[1], "）"), "\n", sep = "")
  got <- dplyr::filter(terms, 結果 == "取得できた")
  missing <- dplyr::filter(terms, 結果 == "取得できなかった")
  cat("\n■ 取得できたもの（", nrow(got), "項目）\n", sep = "")
  print(dplyr::select(got, 区分, 項目, 番号, 値, ページ, 確認状態, Copilotの値), n = Inf, width = Inf)
  cat("\n■ 取得できなかったもの（", nrow(missing), "項目）\n", sep = "")
  print(dplyr::select(missing, 区分, 項目, 確認状態, 理由), n = Inf, width = Inf)

  fpml <- case$fpml %||% NA_character_
  if (!is.na(fpml)) {
    show <- "--show-fpml" %in% args || (interactive() && toupper(trimws(readline("\nFpMLも表示しますか（Y/n）: "))) %in% c("", "Y", "YES"))
    if (show) { cat("\n■ FpML：", normalizePath(fpml, winslash = "/"), "\n", sep = ""); writeLines(readLines(fpml, encoding = "UTF-8", warn = FALSE)) }
  }
}
