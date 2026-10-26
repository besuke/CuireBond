# c1：TRS 確認書1件の処理（R → Claude Code → R）。コンソールには資料から抜き出した条件を表示する
# CURIEプロジェクトの最上位フォルダで実行する。
#
#   source("scripts/run_c1_document.R", encoding = "UTF-8")      # RStudio：資料のURLかファイルのパスを貼る
#   c1_args <- c("--claude=demo", "--claude-demo-dir=<フォルダ>"); source(...)   # RStudio でモードを指定する
#   Rscript scripts/run_c1_document.R <URLまたはファイルのパス> [--claude=api|demo] [--claude-demo-dir=<フォルダ>]
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

# コンソールには、資料（PDF・HTML）から抜き出した条件だけを出す。検証の詳細は r_validation.json に残す
acq <- case$results$manifest$acquisition
cat("\n資料：", acq$name %||% source_arg, if (!is.null(acq)) paste0("（", toupper(acq$format$kind), "、", acq$source_type, "）"), "\n", sep = "")
cat("案件フォルダ：", case$case_dir, "\n", sep = "")
if (!is.null(case$results$failure)) cat("止まった理由：", case$results$failure$kind, "：", case$results$failure$message, "\n", sep = "")

cond <- case$validation$conditions
if (!is.null(cond)) {
  got <- cond |>
    dplyr::filter(.data$value_status == "stated", nzchar(.data$value)) |>
    dplyr::transmute(区分 = .data$section, 項目 = sub("（.*）$", "", .data$label_ja), レッグ = .data$leg_id, 番号 = .data$seq,
                     値 = .data$value, 単位 = .data$unit, ページ = .data$source_page,
                     原文の引用 = dplyr::if_else(nchar(.data$quote) > 60, paste0(substr(.data$quote, 1, 60), "…"), .data$quote))
  cat("\n■ 資料から抜き出した条件（", nrow(got), "項目）\n", sep = "")
  print(got, n = Inf, width = Inf)
  rest <- dplyr::filter(cond, !(.data$value_status == "stated" & nzchar(.data$value)))
  cat("\n資料に記載がなかった・未確定の項目：", nrow(rest), "項目（", case$results$files[["json"]], " に理由つきで記録）\n", sep = "")

  fpml <- case$fpml %||% NA_character_
  if (!is.na(fpml)) cat("\nXML を保存しました：", normalizePath(fpml, winslash = "/"), "\n", sep = "")
  if (!is.null(case$aladdin)) {
    cat("Aladdin 送信用 JSON（下書き・未送信）を保存しました：", normalizePath(case$aladdin$path, winslash = "/"),
        "（未確定 ", nrow(case$aladdin$unresolved), " 項目。理由は aladdin_unresolved.json）\n", sep = "")
  }
}
