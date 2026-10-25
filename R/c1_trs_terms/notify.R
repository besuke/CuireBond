# c1：処理結果の通知。状態（完了／要確認／失敗）、文書、保存先、理由を通知する。
# 通知先（Teams・メール等）は未設定のため、デモの通知（コンソール表示と通知ログ）を使う。
# 通知関数は function(event) の形で、ワークフローの引数として差し替えられる。

make_demo_notifier_c1 <- function(log_path) {
  force(log_path)
  function(event) {
    dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
    event$channel <- "デモ（コンソールと通知ログ。Teams・メール等へは送っていない）"
    cat(jsonlite::toJSON(event, auto_unbox = TRUE, null = "null"), "\n", file = log_path, append = TRUE, sep = "")
    message("[c1 通知（デモ）] ", event$status, "：", event$document_id, "\n  ",
            paste(event$locations, collapse = "\n  "),
            if (length(event$reasons)) paste0("\n  理由：", paste(event$reasons, collapse = "／")) else "")
    invisible(event)
  }
}
