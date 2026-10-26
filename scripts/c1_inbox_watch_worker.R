# c1：inbox の監視（start_c1_inbox_watch() が別の R プロセスとして起動する。直接は実行しない）
#   Rscript scripts/c1_inbox_watch_worker.R <watch/settings.json>
# interval_sec ごとに scan_c1_inbox_once() を呼ぶ。watch/STOP があれば止まる（最長 max_hours 時間）。
# ログ（logs/watch.log）の文言は英語（ログ収集で文字コードが混ざらないように）。

args <- commandArgs(trailingOnly = TRUE)
settings <- jsonlite::fromJSON(args[1])
setwd(settings$project_root)
source("R/load_Curie.R", encoding = "UTF-8")

config <- c1_inbox_config(settings$root, inbox = settings$inbox, processed = settings$processed, review = settings$review,
                          failed = settings$failed, stable_sec = settings$stable_sec, project_root = settings$project_root)
w <- .watch_paths_c1(config)
demo_dir <- if (nzchar(settings$claude_demo_dir)) settings$claude_demo_dir else NULL
say <- function(...) say_Curie(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " ", ...)
c1_inbox_banner(config, paste0("Scan interval: ", settings$interval_sec, " s."))
say("Watcher started.")

snapshot <- .inbox_snapshot_c1(config$inbox)
cycles <- ceiling(settings$max_hours * 3600 / settings$interval_sec)
tryCatch(
  purrr::walk(seq_len(cycles), function(i) {
    if (file.exists(w$stop)) rlang::abort("Stop requested.", class = "c1_watch_stop")
    writeLines(.now_c1(), w$heartbeat)
    r <- tryCatch(scan_c1_inbox_once(config, claude_mode = settings$claude_mode, claude_demo_dir = demo_dir, snapshot = snapshot, banner = FALSE),
                  error = function(e) { say("Scan failed. ", conditionMessage(e)); NULL })
    if (!is.null(r)) {
      snapshot <<- r$snapshot
      if (nrow(r$results) || length(r$skipped)) print(r)
    }
    Sys.sleep(settings$interval_sec)
  }),
  # purrr は中のエラーを包むので、元の条件（c1_watch_stop）までたどって判定する
  error = function(e) if (rlang::cnd_inherits(e, "c1_watch_stop")) say("Stopped on request.") else say("Watcher failed. ", conditionMessage(e)),
  finally = {
    unlink(c(w$lock, w$stop), recursive = TRUE)
    say("Watcher finished.")
  }
)
