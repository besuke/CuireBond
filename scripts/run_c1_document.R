# c1：TRSターム条件PDF 1件の処理（URL入力 → Claude／Copilotの独立読解 → 照合 → 保存 → 通知）
# CURIEプロジェクトの最上位フォルダで実行する。
#
#   Rscript scripts/run_c1_document.R                     # 読むべきURLをコンソールに貼る
#   Rscript scripts/run_c1_document.R <URLまたはファイルのパス> [--no-claude] [--copilot-dir=<フォルダ>] [--show-fpml]
#   source("scripts/run_c1_document.R", encoding = "UTF-8")   # RStudio／Positron（URLを貼る）
#
# AIには元のURL（またはファイル）を渡して自分で読ませる。読めなかった場合は「読めなかった」と記録して終える。
# 入力PDFは SharePoint に配置しない。成果物の保存先はデモ（ローカルフォルダ）。通知はデモ。
# Copilot の出力は --copilot-dir で渡す（未指定なら確認待ち）。

if (!file.exists("CURIE.Rproj")) stop("CURIEプロジェクトの最上位フォルダで実行してください。")
source("R/load_CURIE.R", encoding = "UTF-8")

args <- commandArgs(trailingOnly = TRUE)
source_arg <- if (length(args) && !startsWith(args[1], "--")) args[1] else NA_character_
if (is.na(source_arg)) {
  prompt <- "読むべきURL（またはPDFファイルのパス）を貼ってください: "
  source_arg <- if (interactive()) readline(prompt) else {
    cat(prompt)
    readLines(file("stdin"), n = 1L, warn = FALSE)
  }
  source_arg <- trimws(gsub('^"|"$', "", source_arg))
}
if (!length(source_arg) || !nzchar(source_arg)) stop("URLまたはファイルのパスが入力されていません。", call. = FALSE)
copilot_arg <- sub("^--copilot-dir=", "", grep("^--copilot-dir=", args, value = TRUE))

res <- workflows_trs_document_c1(
  source = source_arg,
  sharepoint = sharepoint_settings_c1(mode = "demo"),
  run_claude = !"--no-claude" %in% args,
  copilot_dir = if (length(copilot_arg)) copilot_arg else NULL
)

cat("\n状態：", res$status, "\n実行フォルダ：", res$run_dir, "\n")
if (!is.null(res$error)) cat("エラー：", res$error, "\n")
if (!is.null(res$summary)) {
  cat("入力：", res$summary$input$file, " SHA-256 ", substr(res$summary$input$sha256, 1, 16), "…\n", sep = "")
  cat("AIに渡した対象：", res$summary$input$sharepoint_url, "（", res$summary$input$sharepoint_connection, "）\n", sep = "")
  cat("AIの出力：", paste(names(res$summary$agents), purrr::map_chr(res$summary$agents, "state"), sep = "=", collapse = "、"), "\n")
  if (!is.null(res$summary$claude_run$cost_usd) && !is.na(res$summary$claude_run$cost_usd)) {
    cat("Claude の費用（API料金換算の目安）：", round(res$summary$claude_run$cost_usd, 3), " USD\n", sep = "")
  }
  if (length(res$summary$reasons)) cat("理由：\n -", paste(res$summary$reasons, collapse = "\n - "), "\n")
}

# そもそもトータル・リターン・スワップか（文書自身の記載で判定。推測しない）
trs <- trs_product_check_c1(res)
cat("\n■ トータル・リターン・スワップか：", trs$answer, sep = "")
if (!is.na(trs$page) || !is.na(trs$quote)) cat("（根拠 p.", trs$page, "「", trs$quote, "」）", sep = "")
cat("　", trs$reason, "\n", sep = "")

# 項目ごとの結果：取得できたもの／取得できなかったもの
terms <- summarize_terms_c1(res)
got <- dplyr::filter(terms, 結果 == "取得できた")
missing <- dplyr::filter(terms, 結果 == "取得できなかった")
cat("\n■ 取得できたもの（", nrow(got), "項目）\n", sep = "")
print(dplyr::select(got, 区分, 項目, 値, ページ, 理由), n = Inf, width = Inf)
cat("\n■ 取得できなかったもの（", nrow(missing), "項目）\n", sep = "")
print(dplyr::select(missing, 区分, 項目, 必須, 理由), n = Inf, width = Inf)

# FpML をコンソールにも表示するか（--show-fpml なら聞かずに表示）
fpml_files <- res$final$fpml_files
if (!is.null(fpml_files) && nrow(fpml_files)) {
  show <- "--show-fpml" %in% args
  if (!show) {
    prompt <- "\nFpMLも表示しますか（Y/n）: "
    ans <- if (interactive()) readline(prompt) else {
      cat(prompt)
      readLines(file("stdin"), n = 1L, warn = FALSE)
    }
    show <- length(ans) == 1L && toupper(trimws(ans)) %in% c("", "Y", "YES")
  }
  if (show) {
    purrr::pwalk(fpml_files, function(kind, fpml_file, ...) {
      cat("\n■ FpML（", kind, "）：", normalizePath(fpml_file, winslash = "/"), "\n", sep = "")
      writeLines(readLines(fpml_file, encoding = "UTF-8", warn = FALSE))
    })
  }
} else {
  cat("\nFpMLは作られていません（AIの抽出結果がない）。\n")
}
