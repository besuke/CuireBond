# c1：Claude Code（claude -p）を R から起動して、原資料を読ませる。
#
# 作業フォルダ（案件フォルダの agents/claude/）に次を置き、Claude にはその原資料を直接読ませる（元の URL から再取得させない）。
#   document.pdf／document.htm … 取得した原資料（そのまま）
#   instructions.md            … 抽出指示書（docs/c1_trs_extraction_instructions.md）
#   prompt.md                  … 依頼文
# Claude が作るファイル：reviewed.json（条件）、fpml.xml（FpML 案）、review_items.md（要確認事項）
# 使えるツールは Read と Write だけ（ウェブ取得なし。読み書きは作業フォルダ内）。PDF を読むのに poppler（pdftoppm）が必要。
#
# モード：api（Claude Code を実行）／demo（用意した結果ファイルを使う。Claude は呼ばない）。黙って切り替えない。
# 起動は processx（引数を配列で渡すので、空白・日本語を含むパスでも崩れない）。タイムアウトで止まる。
# 起動の前に作業フォルダの古い結果を消し、失敗・中断のときは結果ファイルがあっても使わない。

.claude_outputs_c1 <- c("reviewed.json", "fpml.xml", "review_items.md")

claude_prompt_c1 <- function(document) {
  paste(c(
    "# Curie c1：TRS 確認書の読解と FpML 作成",
    "",
    paste0("このフォルダの `", document$local_name, "`（原文）と `instructions.md`（抽出指示）を読んでください。"),
    "すべての項目を原文から読み取り、値・根拠ページ・原文の引用を記録すること。推測で補わないこと。",
    "page は PDF のページ番号。HTML の資料は全体を1ページ（page=1）とし、location に条項名・見出しを書くこと。",
    "記載のある値（value_status=stated）には、必ず原文からの連続した引用（quote）を付けること。",
    "",
    "## 作るファイル（このフォルダに保存）",
    paste0("1. `reviewed.json`：instructions.md の抽出JSONの形式に、項目ごとに次を加える。document は document_id=", document$document_id,
           "、file=", document$file_name, "、pages=", document$pages, "、extracted_by=claude"),
    "   - `leg_id`：trade／return_leg／interest_leg など。`seq`：同じ項目が複数あるときの番号（1から）。1つの値にまとめない",
    "   - `unit`：値の単位（日付は date、スプレッドは bp、下限は percent、日数は business_days、金額は通貨コード、単位がなければ空）",
    "   - `location`：ページ内の位置（条項名・見出しなど。分かる範囲）",
    "   - `review_status`：confirmed（原文で確認した値）／unresolved（空欄・選択肢・解釈不明で未確定）／not_found（記載なし）",
    "   - `review_note`：未確定の理由、解釈に迷った点",
    "   - instructions.md のすべての項目を出すこと",
    "2. `fpml.xml`：reviewed.json の value_status=stated の値だけで作る FpML 5.x confirmation の returnSwap（returnLeg と interestLeg）。",
    "   未確定・記載なしの項目は要素を作らず、XMLコメントに列挙する。推測で埋めない",
    "3. `review_items.md`：要確認事項（未確定事項、解釈に迷った点）と根拠ページの一覧",
    "",
    "## 読めなかった場合",
    "原文を読めなかった場合は、別の手段で読もうとせず、reviewed.json と fpml.xml は作らないこと。",
    "review_items.md の先頭に「読めなかった」と書き、理由を書くこと。",
    "このフォルダ以外のファイルを読み書きしないこと。"
  ), collapse = "\n")
}

# poppler（pdftoppm）のフォルダを探す：環境変数 Curie_POPPLER_BIN → PATH → winget の導入先
.poppler_bin_c1 <- function() {
  on_path <- Sys.which("pdftoppm")
  if (nzchar(on_path)) return(dirname(on_path))
  winget <- Sys.glob(file.path(Sys.getenv("LOCALAPPDATA"), "Microsoft", "WinGet", "Packages", "oschwartz10612.Poppler*",
                               "poppler-*", "Library", "bin"))
  candidates <- c(Sys.getenv("Curie_POPPLER_BIN", ""), winget)
  purrr::detect(candidates, \(d) nzchar(d) && file.exists(file.path(d, "pdftoppm.exe"))) %||% NA_character_
}

# Claude Code の実行ファイル（環境変数 Curie_CLAUDE_CMD で指定できる）
claude_command_c1 <- function() {
  cmd <- Sys.getenv("Curie_CLAUDE_CMD", "")
  if (nzchar(cmd)) return(cmd)
  found <- Sys.which("claude")
  if (nzchar(found)) return(unname(found))
  home <- file.path(Sys.getenv("USERPROFILE"), ".local", "bin", "claude.exe")
  if (file.exists(home)) home else NA_character_
}

claude_args_c1 <- function(document) {
  c("-p", paste0("Read prompt.md in the current folder and follow it exactly. Read ", document$local_name,
                 " yourself; if you cannot read it, write only review_items.md explaining why. ",
                 "Otherwise create reviewed.json, fpml.xml and review_items.md in this folder."),
    "--output-format", "stream-json", "--verbose", "--allowedTools", "Read", "Write", "--permission-mode", "acceptEdits")
}

# stream-json の1行 → ログに出す英語の文言（なければ空）。開始、読んだファイル、書いたファイル、使ったツール
.claude_events_c1 <- function(line) {
  x <- tryCatch(jsonlite::fromJSON(line, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.list(x) || is.null(x$type)) return(character())
  if (identical(x$type, "system") && identical(x$subtype, "init")) return("session started")
  if (!identical(x$type, "assistant")) return(character())
  tools <- purrr::keep(x$message$content %||% list(), \(c) identical(c$type, "tool_use"))
  purrr::map_chr(tools, function(t) {
    file <- basename(t$input$file_path %||% "")
    switch(t$name, Read = paste0("read ", file), Write = paste0("wrote ", file), paste0("used ", t$name))
  })
}

# Claude の標準出力（stream-json の各行、または1つの JSON）から、結果の行（type = result）を取り出す
.claude_result_c1 <- function(stdout) {
  lines <- strsplit(stdout, "\r?\n")[[1]]
  rows <- purrr::map(lines[nzchar(lines)], \(l) tryCatch(jsonlite::fromJSON(l, simplifyVector = FALSE), error = function(e) NULL))
  purrr::keep(rows, \(r) is.list(r) && identical(r$type, "result")) |> dplyr::last(default = NULL)
}

# 作業フォルダを用意する（古い結果は消す）
prepare_claude_workdir_c1 <- function(agent_dir, document, source_path, instruction_path) {
  dir.create(agent_dir, recursive = TRUE, showWarnings = FALSE)
  unlink(file.path(agent_dir, c(.claude_outputs_c1, "claude_stdout.txt", "claude_stderr.txt")))
  file.copy(source_path, file.path(agent_dir, document$local_name), overwrite = TRUE)
  file.copy(instruction_path, file.path(agent_dir, "instructions.md"), overwrite = TRUE)
  writeLines(claude_prompt_c1(document), file.path(agent_dir, "prompt.md"), useBytes = TRUE)
  invisible(agent_dir)
}

# Claude Code を実行する。結果：state（ok／failed／timeout）と記録（終了コード・秒数・費用の目安など）
# cmd・args はテストで差し替えられる（既定は claude -p ...）
run_claude_c1 <- function(agent_dir, document, cmd = claude_command_c1(), args = claude_args_c1(document),
                          timeout_sec = as.numeric(Sys.getenv("Curie_CLAUDE_TIMEOUT_SEC", "1800"))) {
  unlink(file.path(agent_dir, .claude_outputs_c1))   # 古い結果を成功結果として使わない
  log <- list(mode = "api", command = basename(cmd %||% ""), timeout_sec = timeout_sec,
              started_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"))
  if (is.na(cmd) || !nzchar(cmd)) {
    return(list(state = "failed", log = c(log, error = "Claude Code (claude) was not found. Set Curie_CLAUDE_CMD.")))
  }
  env <- "current"
  if (identical(document$kind, "pdf")) {
    bin <- .poppler_bin_c1()
    if (is.na(bin)) return(list(state = "failed", log = c(log, error = "pdftoppm (poppler) is missing, so Claude cannot read the PDF.")))
    env <- c("current", PATH = paste(normalizePath(bin), Sys.getenv("PATH"), sep = ";"))
  }
  t0 <- Sys.time()
  # Claude の動き（資料を読んだ・ファイルを書いた）を、その都度ログに出す（stream-json の1行ごと）
  progress <- function(line, proc) {
    purrr::walk(.claude_events_c1(line), \(m) message(format(Sys.time(), "%H:%M:%S"), " [c1] Claude: ", m,
                                                      " (", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), " s)"))
  }
  res <- tryCatch(processx::run(cmd, args, wd = agent_dir, timeout = timeout_sec, error_on_status = FALSE,
                                env = env, encoding = "UTF-8", stdout_line_callback = progress),
                  error = function(e) list(status = NA_integer_, stdout = "", stderr = conditionMessage(e), timeout = FALSE))
  log$seconds <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")))
  log$exit_status <- res$status
  log$timed_out <- isTRUE(res$timeout)
  writeLines(res$stdout %||% "", file.path(agent_dir, "claude_stdout.txt"), useBytes = TRUE)
  writeLines(res$stderr %||% "", file.path(agent_dir, "claude_stderr.txt"), useBytes = TRUE)
  # 最後の結果の行（type = result）から、費用の目安・回数・エラーの有無を読む
  run <- .claude_result_c1(res$stdout %||% "")
  log$cost_usd <- run$total_cost_usd %||% NA_real_
  log$num_turns <- run$num_turns %||% NA_integer_
  log$is_error <- run$is_error %||% NA
  state <- if (log$timed_out) "timeout" else if (!identical(as.integer(res$status), 0L) || isTRUE(log$is_error)) "failed" else "ok"
  if (state != "ok") {
    # 失敗・中断：途中で書かれた結果ファイルは使わない（別名で残して調べられるようにする）
    purrr::walk(.claude_outputs_c1, \(f) { p <- file.path(agent_dir, f); if (file.exists(p)) file.rename(p, paste0(p, ".incomplete")) })
    log$error <- if (state == "timeout") paste0("Claude was stopped after the timeout (", timeout_sec, " s).") else
      paste0("Claude failed (exit code ", res$status, ")", if (nzchar(res$stderr %||% "")) paste0(": ", substr(res$stderr, 1, 300)) else "")
  }
  list(state = state, log = log)
}

# demo：用意した結果ファイルを作業フォルダへ置く（Claude は呼ばない）
run_claude_demo_c1 <- function(agent_dir, demo_dir) {
  unlink(file.path(agent_dir, .claude_outputs_c1))
  log <- list(mode = "demo", demo_dir = demo_dir, started_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"))
  if (is.null(demo_dir) || !dir.exists(demo_dir)) {
    return(list(state = "failed", log = c(log, error = paste0("Demo result folder not found: ", demo_dir %||% "not specified"))))
  }
  purrr::walk(.claude_outputs_c1, \(f) if (file.exists(file.path(demo_dir, f))) file.copy(file.path(demo_dir, f), file.path(agent_dir, f), overwrite = TRUE))
  list(state = "ok", log = log)
}

# Claude の結果の形式チェック：state は ok／unreadable（読めなかった）／missing（結果がない）／invalid（形式が不正）
check_claude_outputs_c1 <- function(agent_dir) {
  p <- rlang::set_names(file.path(agent_dir, .claude_outputs_c1), c("reviewed", "fpml", "review"))
  if (!file.exists(p[["reviewed"]])) {
    head <- if (file.exists(p[["review"]])) paste(utils::head(readLines(p[["review"]], encoding = "UTF-8", warn = FALSE), 5), collapse = " ") else ""
    return(if (grepl("読めなかった", head)) list(state = "unreadable", issues = "Claude reported that it could not read the document (review_items.md).")
           else list(state = "missing", issues = "Claude output (reviewed.json) is missing."))
  }
  x <- tryCatch(jsonlite::fromJSON(p[["reviewed"]], simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(x)) return(list(state = "invalid", issues = "reviewed.json is not valid JSON."))
  fields <- x$fields %||% list()
  issues <- c(
    if (!length(fields)) "fields is missing",
    unlist(purrr::imap(fields, function(f, i) c(
      purrr::map_chr(purrr::keep(c("field", "value_status", "review_status"), \(k) is.null(f[[k]])), \(k) paste0("fields[", i, "] has no ", k)),
      if (!is.null(f$value_status) && !f$value_status %in% .value_status_levels_c1) paste0("fields[", i, "] has an invalid value_status: ", f$value_status)
    ))),
    if (!file.exists(p[["fpml"]])) "fpml.xml is missing"
  )
  if (length(issues)) list(state = "invalid", issues = issues) else list(state = "ok", reviewed = x, issues = character())
}
