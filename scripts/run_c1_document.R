# c1：TRS 確認書1件の処理（Claude の読解・FpML 作成 → R の検証）
# CURIEプロジェクトの最上位フォルダで実行する。
#
#   source("scripts/run_c1_document.R", encoding = "UTF-8")      # RStudio：資料のURLかファイルのパスを貼る
#   c1_args <- c("--claude=demo", "--claude-demo-dir=<フォルダ>"); source(...)   # RStudio でモードを指定する
#   Rscript scripts/run_c1_document.R <URLまたはファイルのパス> [--claude=api|demo] [--claude-demo-dir=<フォルダ>] [--show-fpml]
#
# 入力：公開 PDF・HTML の URL、ローカルの PDF・HTML（Box Drive の同期フォルダのファイルも可。形式は自動で判定する）。
# 成果物：result/conditions.json と FpML（検証済みなら fpml_final.xml、それ以外は fpml_DRAFT.xml）。

if (!file.exists("CURIE.Rproj")) stop("CURIEプロジェクトの最上位フォルダで実行してください。")
source("R/load_CURIE.R", encoding = "UTF-8")

# RStudio で source() するときは、事前に c1_args <- c("--claude=demo", ...) と書けば同じ指定ができる
args <- if (exists("c1_args", envir = globalenv())) get("c1_args", envir = globalenv()) else commandArgs(trailingOnly = TRUE)
opt <- function(name, default = NULL) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit)) sub(paste0("^--", name, "="), "", hit[1]) else default
}
source_arg <- if (length(args) && !startsWith(args[1], "--")) args[1] else NA_character_
if (is.na(source_arg)) {
  prompt <- "資料のURL（またはファイルのパス）を貼ってください: "
  source_arg <- if (interactive()) readline(prompt) else { cat(prompt); readLines(file("stdin"), n = 1L, warn = FALSE) }
  source_arg <- trimws(gsub('^"|"$', "", source_arg))
}
if (!length(source_arg) || !nzchar(source_arg)) stop("URLまたはファイルのパスが入力されていません。", call. = FALSE)

claude_mode <- opt("claude", "api")
cat("\nClaude：", claude_mode, if (claude_mode == "demo") "（デモ：用意した結果を使う。Claude は呼ばない）", "\n", sep = "")

case <- workflows_c1_case(source_arg, claude_mode, claude_demo_dir = opt("claude-demo-dir"))

cat("\n状態：", case$state, "\n案件フォルダ：", case$case_dir, "\n", sep = "")
if (!is.null(case$results$manifest$stages)) {
  st <- case$results$manifest$stages
  cat("段階：資料 ", st$document, "／読解用データ ", st$reading_data %||% "—", "／Claude ", st$claude,
      "／成果物 ", st$outputs, "\n", sep = "")
  acq <- case$results$manifest$acquisition
  if (!is.null(acq)) {
    cat("取得：", acq$source_type, "、", acq$format$kind, "、HTTP ", acq$http_status %||% "—", "、", acq$retrieved_at,
        "、SHA-256 ", substr(acq$sha256, 1, 12), "…\n", sep = "")
  }
}
if (!is.null(case$error)) cat("エラー：", case$error, "\n")
json <- case$results$files[["json"]] %||% NA_character_
if (!is.na(json)) cat("JSON：", json, "\n", sep = "")
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
  print(dplyr::select(got, 区分, 項目, 番号, 値, ページ, 確認状態), n = Inf, width = Inf)
  cat("\n■ 取得できなかったもの（", nrow(missing), "項目）\n", sep = "")
  print(dplyr::select(missing, 区分, 項目, 確認状態, 理由), n = Inf, width = Inf)

  fpml <- case$fpml %||% NA_character_
  if (!is.na(fpml)) {
    show <- "--show-fpml" %in% args || (interactive() && toupper(trimws(readline("\nFpMLも表示しますか（Y/n）: "))) %in% c("", "Y", "YES"))
    if (show) { cat("\n■ FpML：", normalizePath(fpml, winslash = "/"), "\n", sep = ""); writeLines(readLines(fpml, encoding = "UTF-8", warn = FALSE)) }
  }
}
