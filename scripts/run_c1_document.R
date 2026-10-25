# c1：TRSターム条件PDF 1件の処理（取得 → SharePoint配置 → Claude／Copilotの独立読解 → 照合 → 保存 → 通知）
# CURIEプロジェクトの最上位フォルダで実行する。
#
#   Rscript scripts/run_c1_document.R <URLまたはファイルのパス> [--no-claude] [--copilot-dir=<フォルダ>]
#
# SharePoint はデモ（ローカルフォルダ）。接続情報の提示後に sharepoint_settings_c1(mode = "microsoft365r", ...) に替える。
# Copilot の出力は --copilot-dir で渡す（未指定なら確認待ち）。通知はデモ（コンソールと通知ログ）。

if (!file.exists("CURIE.Rproj")) stop("CURIEプロジェクトの最上位フォルダで実行してください。")
source("R/load_CURIE.R", encoding = "UTF-8")

args <- commandArgs(trailingOnly = TRUE)
source_arg <- if (length(args) && !startsWith(args[1], "--")) args[1] else
  "https://www.sc.com/en/uploads/sites/66/content/docs/Total-Return-Swap.pdf"
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
  cat("入力PDFの共有先：", res$summary$input$sharepoint_url, "（", res$summary$input$sharepoint_connection, "）\n", sep = "")
  cat("AIの出力：", paste(names(res$summary$agents), purrr::map_chr(res$summary$agents, "state"), sep = "=", collapse = "、"), "\n")
  if (length(res$summary$reasons)) cat("理由：\n -", paste(res$summary$reasons, collapse = "\n - "), "\n")
}
