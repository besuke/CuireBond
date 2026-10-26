# c1：フォルダー投入型の処理。監視フォルダーに置いた PDF を1件ずつ既存の c1（workflows_c1_case）で読解・検証し、
# 案件ごとに PDF と3種類の成果物（TSV・FpML・Aladdin の payload）を保存する。
#
# フォルダー（既定では root の直下に置いた PDF を処理する。サブフォルダーは監視しない。
# inbox・processed・review・failed は c1_inbox_config() で別の場所にもできる）
#   <root>/*.pdf   処理する PDF。出力の保存がすべて終わるまで、元の PDF は削除も移動もしない
#   processed/     保存済みの案件：共通名のフォルダーに 共通名.pdf・共通名.tsv・共通名.xml・共通名_payload.json・validation.json
#                  （保存が終わってから、元の PDF を root から消す＝移す）
#   review/        要確認の案件：名前に要る取引日・取引最終日・相手先が不明（推測しない。不明な部分は UNKNOWN）
#   failed/        失敗の理由（*.reason.json）。元の PDF は root に残し、次のスキャンからは処理しない（再処理は retry_failed = TRUE）
#   duplicates/    処理済みの PDF と同じ内容（SHA-256 が同じ）のもの。AI は呼ばない
#   work/          作業用（1件ごとの job フォルダー。終われば消える）
#   locks/         処理中の PDF の SHA-256。ロックを取れた処理だけがその PDF を扱う（並列にしても同じ入力を二重に取らない）
#   history.jsonl  処理履歴（元のファイル名・SHA-256・状態・保存後の名前・保存先・理由）。1行1件の追記
#
# 共通名：TRS_取引日_取引最終日_カウンターパーティ名の先頭5文字（日付は YYYYMMDD）
#   取引日は trade date、取引最終日は termination date。カウンターパーティは自社でない側の当事者
#   （自社の名前は対応表 Curie_ALADDIN_MAP の our_party_names。なければ不明＝要確認）
# FpML・TSV・payload は、同じ抽出・検証済みのデータから作る（成果物ごとに AI を呼び直さない）。Aladdin へは送信しない。

# ---- 設定 -----------------------------------------------------------------------------------
# フォルダーはすべて絶対パスにする（作業ディレクトリに依存させない）
c1_inbox_config <- function(root = Sys.getenv("Curie_C1_INBOX_ROOT", ""), inbox = NULL, processed = NULL, review = NULL,
                            failed = NULL, stable_sec = 10, project_root = Sys.getenv("Curie_ROOT", "")) {
  if (!nzchar(root)) stop("The watched folder is not set. Use c1_inbox_config(root = ...) or set Curie_C1_INBOX_ROOT.", call. = FALSE)
  if (!nzchar(project_root) || !file.exists(file.path(project_root, "Curie.Rproj"))) {
    stop("The Curie project folder is unknown. Load it with source(\"R/load_Curie.R\") or set project_root.", call. = FALSE)
  }
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  dirs <- list(inbox = inbox %||% root, work = file.path(root, "work"),
               processed = processed %||% file.path(root, "processed"), review = review %||% file.path(root, "review"),
               failed = failed %||% file.path(root, "failed"), duplicates = file.path(root, "duplicates"),
               locks = file.path(root, "locks"), logs = file.path(root, "logs"))
  purrr::walk(dirs, \(d) dir.create(d, recursive = TRUE, showWarnings = FALSE))
  project_root <- normalizePath(project_root, winslash = "/")
  c(purrr::map(dirs, \(d) normalizePath(d, winslash = "/", mustWork = FALSE)),
    list(root = root, history = file.path(root, "history.jsonl"), events = file.path(dirs$logs, "events.jsonl"),
         stable_sec = stable_sec, project_root = project_root,
         instruction_path = file.path(project_root, "docs", "c1_trs_extraction_instructions.md")))
}

# ---- 共通名 ---------------------------------------------------------------------------------
# 名前の一部：前後の空白を除き、先頭 n 文字を取り、Windows で使えない文字（<>:"/\|?* と制御文字、末尾の空白・ピリオド）を _ にする
.safe_name_part_c1 <- function(x, n = 5L) {
  if (is.na(x)) return(NA_character_)
  x <- stringr::str_sub(stringr::str_trim(x), 1L, n)
  x <- stringr::str_replace_all(x, "[<>:\"/\\\\|?*[:cntrl:]]", "_")
  x <- stringr::str_replace(x, "[ .]+$", \(m) strrep("_", nchar(m)))
  if (nzchar(x)) x else NA_character_
}

# 抽出・確認済みの条件から共通名を作る。不明な部分があれば name は NA（要確認。review_name は不明な部分を UNKNOWN にした名前）
c1_common_name <- function(conditions, aladdin_map = read_aladdin_map_c1()) {
  g <- .condition_getter_c1(conditions)
  ymd <- function(id) {
    d <- suppressWarnings(as.Date(g(id), format = "%Y-%m-%d"))
    if (is.na(d)) NA_character_ else format(d, "%Y%m%d")
  }
  side <- .our_side_c1(g, aladdin_map)
  cp <- switch(side %|% "none", receive_return = g("trade.total_return_payer"), pay_return = g("trade.total_return_receiver"), NA_character_)
  parts <- c(取引日 = ymd("trade.trade_date"), 取引最終日 = ymd("trade.termination_date"), カウンターパーティ = .safe_name_part_c1(cp))
  missing <- names(parts)[is.na(parts)]
  reasons <- c(取引日 = "trade date not confirmed in the document", 取引最終日 = "termination date not confirmed in the document",
               カウンターパーティ = if (is.na(side)) "counterparty unknown: our party (our_party_names) does not match either party"
                                    else "counterparty name not confirmed in the document")
  list(name = if (length(missing)) NA_character_ else paste(c("TRS", parts), collapse = "_"),
       review_name = paste(c("TRS", dplyr::coalesce(parts, "UNKNOWN")), collapse = "_"),
       parts = as.list(parts), counterparty = cp %|% NA_character_, missing = as.list(reasons[missing]))
}

# 保存先に共通名のフォルダーを予約する（dir.create は既にあれば FALSE。上書きせず、衝突したときだけ _2、_3 … を付ける）
.reserve_dir_c1 <- function(parent, name) {
  hit <- purrr::detect(c(name, paste0(name, "_", 2:999)), \(n) dir.create(file.path(parent, n), showWarnings = FALSE))
  if (is.null(hit)) stop("Cannot create the destination folder.\n", path_line_Curie(file.path(parent, name), "dir"), call. = FALSE)
  hit
}

# ---- 成果物 ---------------------------------------------------------------------------------
# 縦持ちの表（1項目・1レッグ・1期間番号で1行）。既存の抽出結果と検証結果を並べる
c1_conditions_table <- function(validation) {
  ok_ng <- function(x) dplyr::case_when(is.na(x) ~ "", x ~ "合格", .default = "不合格")
  fpml <- validation$checks |>
    dplyr::filter(.data$区分 == "FpML", startsWith(.data$検証, "条件との整合：")) |>
    dplyr::transmute(item_id = sub("（.*$", "", sub("^条件との整合：", "", .data$検証)),
                     seq = as.integer(dplyr::coalesce(stringr::str_match(.data$検証, "（(\\d+)番目）")[, 2], "1")),
                     FpMLとの整合 = .data$結果) |>
    dplyr::distinct(.data$item_id, .data$seq, .keep_all = TRUE)
  validation$conditions |>
    dplyr::mutate(seq = as.integer(.data$seq)) |>
    dplyr::left_join(fpml, by = c("item_id", "seq")) |>
    dplyr::transmute(
      項目ID = .data$item_id, 項目 = .data$label_ja, 区分 = .data$section, レッグ = .data$leg_id, 期間番号 = .data$seq,
      値 = .data$value, 単位 = .data$unit, 記載状況 = .data$value_status, 確認状態 = .data$review_status,
      ページ = .data$source_page, 原文根拠 = .data$quote,
      型の確認 = ok_ng(.data$型の確認), 単位の確認 = ok_ng(.data$単位の確認), 原文根拠の確認 = .data$引用の実在確認,
      FpMLとの整合 = dplyr::coalesce(.data$FpMLとの整合, ""),
      検証結果 = dplyr::case_when(
        .data$value_status != "stated" ~ "記載なし・未確定",
        .data$review_status != "confirmed" ~ "要確認",
        .data$型の確認 %in% FALSE | .data$単位の確認 %in% FALSE | .data$FpMLとの整合 %in% "不合格" ~ "不合格",
        !.data$引用の実在確認 %in% .quote_ok_c1 | .data$FpMLとの整合 %in% "未検証" ~ "要確認",
        .default = "合格"),
      備考 = .data$review_note)
}

# 縦持ちの表のファイル（パイプ | 区切り。UTF-8・BOM 付きで Excel でも文字化けしない）。
# 列がずれないように、値の中の | は / に、タブ・改行は空白にする
.write_tsv_c1 <- function(df, path) {
  clean <- function(x) gsub("[\t\r\n]+", " ", gsub("|", "/", dplyr::coalesce(as.character(x), ""), fixed = TRUE))
  lines <- c(paste(names(df), collapse = "|"),
             purrr::pmap_chr(dplyr::mutate(df, dplyr::across(dplyr::everything(), clean)), \(...) paste(c(...), collapse = "|")))
  con <- file(path, "wb")
  on.exit(close(con))
  writeBin(c(as.raw(c(0xEF, 0xBB, 0xBF)), charToRaw(enc2utf8(paste0(paste(lines, collapse = "\r\n"), "\r\n")))), con)
  invisible(path)
}

# 1件の成果物を out_dir に作る（すべて同じ case＝同じ抽出・検証済みデータから。AI は呼ばない）。
# extraction/ には Claude の抽出結果（reviewed.json・fpml.xml・review_items.md）を残す（AI を呼ばずに作り直すため）
.write_c1_case_outputs <- function(case, out_dir, name, naming, info) {
  dir.create(file.path(out_dir, "extraction"), recursive = TRUE, showWarnings = FALSE)
  p <- function(suffix) file.path(out_dir, paste0(name, suffix))
  if (!file.copy(info$pdf, p(".pdf"))) stop("Cannot copy the PDF.", call. = FALSE)
  .write_tsv_c1(c1_conditions_table(case$validation), p(".tsv"))
  if (is.na(case$fpml) || !file.copy(case$fpml, p(".xml"))) stop("FpML is missing.", call. = FALSE)
  if (is.null(case$aladdin) || !file.copy(case$aladdin$path, p("_payload.json"))) stop("Payload is missing.", call. = FALSE)
  extracted <- file.path(case$case_dir, "agents", "claude", .claude_outputs_c1)
  if (!all(file.copy(extracted[file.exists(extracted)], file.path(out_dir, "extraction")))) stop("Cannot copy the extraction results.", call. = FALSE)
  v <- case$validation
  unconfirmed <- v$conditions |>
    dplyr::filter(!(.data$value_status == "stated" & .data$review_status == "confirmed")) |>
    dplyr::select("item_id", "leg_id", "seq", "value", "value_status", "review_status", "review_note")
  jsonlite::write_json(list(
    name = name, status = info$status, original_name = info$original, sha256 = info$sha256, job_id = info$job_id,
    created_at = .now_c1(), naming = naming[c("parts", "counterparty", "missing")],
    files = list(pdf = basename(p(".pdf")), tsv = basename(p(".tsv")), fpml = basename(p(".xml")), payload = basename(p("_payload.json"))),
    fpml_status = if (identical(basename(case$fpml), "fpml_final.xml")) "検証済み" else "DRAFT（要確認）",
    payload_status = "DRAFT / NOT SENT",
    state = v$state, verdicts = v$verdicts, checks = v$checks,
    unconfirmed_items = unconfirmed, aladdin_unresolved = case$aladdin$unresolved, aladdin_conversions = case$aladdin$conversions,
    review_items = jsonlite::read_json(file.path(case$case_dir, "result", "conditions.json"))$review_items %||% ""),
    file.path(out_dir, "validation.json"), auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)
  list.files(out_dir, full.names = TRUE)   # ファイルと extraction フォルダー
}

# 読解・検証済みの case を、共通名で保存先（名前が決まれば processed、不明なら review）に保存する。
# 保存先のフォルダーを先に予約し（上書きしない）、途中で失敗したら予約したフォルダーを消してエラーにする
.save_c1_case <- function(case, config, naming, work_dir, info) {
  status <- if (is.na(naming$name)) "要確認" else "保存済み"
  parent <- if (status == "保存済み") config$processed else config$review
  name <- .reserve_dir_c1(parent, naming$name %|% naming$review_name)
  dest <- file.path(parent, name)
  tryCatch({
    out <- .write_c1_case_outputs(case, file.path(work_dir, "out"), name, naming, c(info, list(status = status)))
    copied <- file.copy(out, dest, recursive = TRUE)
    if (!all(copied)) stop("Cannot copy outputs to the destination: ", paste(basename(out[!copied]), collapse = ", "), call. = FALSE)
  }, error = function(e) {
    unlink(dest, recursive = TRUE)
    stop(e)
  })
  list(status = status, name = name, dest = dest)
}

# ---- 履歴 -----------------------------------------------------------------------------------
.append_history_c1 <- function(config, entry) {
  cat(jsonlite::toJSON(entry, auto_unbox = TRUE), "\n", file = config$history, append = TRUE, sep = "")
}

read_c1_history <- function(config) {
  empty <- tibble::tibble(time = character(), scan_id = character(), job_id = character(), original_name = character(),
                          sha256 = character(), status = character(), saved_name = character(), saved_dir = character(), reason = character())
  if (!file.exists(config$history)) return(empty)
  lines <- readLines(config$history, encoding = "UTF-8", warn = FALSE)
  dplyr::bind_rows(empty, purrr::map(lines[nzchar(lines)], \(l) tibble::as_tibble(jsonlite::fromJSON(l))) |> purrr::list_rbind())
}

# ---- 通知（PDF の検出・処理開始・完了・要確認・重複・失敗） ----------------------------------------------
# 監視は別の R プロセスで動くので、出来事を logs/events.jsonl に1行ずつ書き、RStudio のセッションが読んで表示する
# （follow_c1_inbox_watch）。1回のスキャンでは、同じ出来事をその場で表示する（print.c1_inbox_scan）。
# summary：完了時の取引の要約（CP・Start・End・Pay leg・Receive leg）。path・kind：表示する保存先（dir／file）
.c1_event <- function(config, event, file, scan_id = "", path = "", kind = "file", reason = "", summary = NULL) {
  ev <- list(time = format(Sys.time(), "%H:%M:%S"), scan_id = scan_id, event = event, file = file,
             path = if (nzchar(path)) path_display_Curie(path) else "", kind = kind, reason = reason,
             summary = if (length(summary)) as.list(summary) else NULL)
  cat(jsonlite::toJSON(ev, auto_unbox = TRUE, null = "null"), "\n", file = config$events, append = TRUE, sep = "")
}

# 出来事の表示（英語）：時刻・文言・元のファイル名（理由）→ 取引の要約 → 改行して dir:／file: で保存先
.say_c1_event <- function(ev) {
  # 種類が一目で分かる印（絵文字は ASCII 外でログ収集時に文字化けしうるので、ASCII の記号にする）
  title <- c(detected = "[+]  PDF detected.", started = "[>]  Processing started.", completed = "[OK] Processing completed.",
             review = "[!]  Saved for review.", duplicate = "[=]  Duplicate skipped.", failed = "[X]  Processing failed.")[[ev$event]]
  # 検出・処理開始・失敗はお知らせ（message）、完了・要確認・重複は結果（cat）として出す
  out <- if (ev$event %in% c("detected", "started", "failed")) note_Curie else say_Curie
  # 出来事ごとに空行を入れて区切る
  out("\n", ev$time, " ", title, " ", ev$file, if (nzchar(ev$reason %||% "")) paste0(" (", ev$reason, ")"))
  if (length(ev$summary)) {
    s <- unlist(ev$summary)
    out(paste0(sprintf("%-13s", paste0(names(s), ":")), s, collapse = "\n"))
  }
  if (nzchar(ev$path %||% "")) out(ev$kind, ":", ev$path)
}

# events.jsonl の行（書きかけの最後の行は読まない）
.read_c1_events <- function(config) {
  if (!file.exists(config$events)) return(list())
  rows <- purrr::map(readLines(config$events, encoding = "UTF-8", warn = FALSE),
                     \(l) tryCatch(jsonlite::fromJSON(l, simplifyVector = FALSE), error = function(e) NULL))
  ok <- purrr::map_lgl(rows, Negate(is.null))
  rows[seq_len(if (all(ok)) length(rows) else which(!ok)[1] - 1L)]
}

# ---- 1件の処理 ------------------------------------------------------------------------------
# PDF 1件を処理し、結果を1行の表で返す（状態：保存済み／要確認／重複／失敗／スキップ）。
# グローバル変数や共有の一時ファイルは使わない（引数と自分の job フォルダーだけを使う）ので、並列のワーカーからも呼べる。
# 元の PDF は読むだけ。出力の保存がすべて終わってから消す（失敗したときは元の場所に残す）。
# claude_demo_dir：デモ用。フォルダー、または元のファイル名を受け取ってフォルダーを返す関数
process_c1_pdf <- function(pdf, config, claude_mode = .reader_modes_c1, claude_demo_dir = NULL,
                           aladdin_map = read_aladdin_map_c1(), xsd_path = Sys.getenv("Curie_FPML_XSD", ""),
                           timeout_sec = as.numeric(Sys.getenv("Curie_CLAUDE_TIMEOUT_SEC", "1800")),
                           claude_cmd = claude_command_c1(), scan_id = "") {
  claude_mode <- match.arg(claude_mode)
  original <- basename(pdf)
  job_id <- paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_", paste(as.character(openssl::rand_bytes(4)), collapse = ""))
  row <- function(status, sha = "", saved_name = "", saved_dir = "", reason = "") {
    tibble::tibble(time = .now_c1(), scan_id = scan_id, job_id = job_id, original_name = original, sha256 = sha,
                   status = status, saved_name = saved_name, saved_dir = saved_dir, reason = reason)
  }
  gone <- "file no longer exists (already saved by another process)"
  if (!file.exists(pdf)) return(row("スキップ", reason = gone))
  sha <- pdf_sha256_c1(pdf)
  record <- function(...) {
    r <- row(..., sha = sha)
    .append_history_c1(config, as.list(r))
    r
  }

  # 1. ロック：同じ内容の PDF を同時に処理しない（取れた処理だけが扱う。取れなければ次のスキャンで再確認。履歴には残さない）
  lock <- file.path(config$locks, sha)
  if (!dir.create(lock, showWarnings = FALSE)) return(row("スキップ", sha = sha, reason = "a PDF with the same content is being processed"))
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  if (!file.exists(pdf)) return(row("スキップ", sha = sha, reason = gone))

  # 2. 処理済みと同じ内容：AI を呼ばず duplicates へ移す（その内容の成果物は保存済み）
  # 作り直した案件もあるので、同じ内容の最後の保存先を使う
  done <- read_c1_history(config) |> dplyr::filter(.data$sha256 == .env$sha, .data$status %in% c("保存済み", "要確認")) |>
    dplyr::slice_tail(n = 1)
  if (nrow(done)) {
    kept <- file.path(config$duplicates, paste0(job_id, "_", original))
    if (!file.rename(pdf, kept) && file.copy(pdf, kept)) file.remove(pdf)
    reason <- paste0("same content as ", done$original_name, "; AI not called")
    .c1_event(config, "duplicate", original, scan_id, path = done$saved_dir, kind = "dir", reason = reason)
    return(record("重複", saved_name = done$saved_name, saved_dir = done$saved_dir, reason = reason))
  }
  .c1_event(config, "started", original, scan_id)

  # 3. 読解・検証 → 共通名 → 作業フォルダーで成果物 → 保存先へコピー → すべて終わってから元の PDF を消す
  job_dir <- file.path(config$work, job_id)
  dir.create(job_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(job_dir, recursive = TRUE), add = TRUE)
  tryCatch({
    demo_dir <- if (is.function(claude_demo_dir)) claude_demo_dir(original) else claude_demo_dir
    # 取得の段階で元の PDF を job フォルダーへコピーする（元の PDF は読むだけ）
    case <- workflows_c1_case(pdf, claude_mode, case_id = "case", output_root = job_dir, claude_demo_dir = demo_dir,
                              xsd_path = xsd_path, instruction_path = config$instruction_path, timeout_sec = timeout_sec,
                              claude_cmd = claude_cmd, aladdin_map = aladdin_map)
    f <- case$results$failure
    if (!is.null(f)) stop(.failure_kind_en_c1(f$kind), ". ", f$message, call. = FALSE)
    naming <- c1_common_name(case$validation$conditions, aladdin_map)
    # 途中で失敗すれば .save_c1_case が保存先を消す。元の PDF はそのまま残る
    saved <- .save_c1_case(case, config, naming, job_dir,
                           list(pdf = case$results$source_path, original = original, sha256 = sha, job_id = job_id))
    removed <- file.remove(pdf)
    reason <- paste(c(if (saved$status == "要確認") unlist(naming$missing),
                      if (!removed) "original PDF could not be deleted (moved to duplicates on the next scan)"), collapse = "; ")
    .c1_event(config, if (saved$status == "要確認") "review" else "completed", original, scan_id, path = saved$dest, kind = "dir",
              reason = reason, summary = deal_summary_c1(case$validation$conditions, aladdin_map))
    record(saved$status, saved_name = saved$name, saved_dir = saved$dest, reason = reason)
  }, error = function(e) {
    reason_file <- file.path(config$failed, paste0(job_id, "_", original, ".reason.json"))
    jsonlite::write_json(list(original_name = original, original_path = pdf, sha256 = sha, job_id = job_id, time = .now_c1(),
                              reason = conditionMessage(e)), reason_file, auto_unbox = TRUE, pretty = TRUE)
    .c1_event(config, "failed", original, scan_id, path = reason_file, kind = "file", reason = conditionMessage(e))
    record("失敗", saved_dir = reason_file, reason = conditionMessage(e))
  })
}

# ---- フォルダーの処理 -------------------------------------------------------------------------
# 処理に使うワーカーの数：purrr::walk なら 1（逐次）、furrr::future_walk などなら future の plan のワーカー数
.workers_c1 <- function(.walk = purrr::walk) {
  if (identical(.walk, purrr::walk)) 1L else future::nbrOfWorkers()
}

# 起動メッセージ：どのフォルダーを監視し、どこへ保存するか、処理に使うワーカー（コア）の数（extra：追加の行）
c1_inbox_banner <- function(config, extra = NULL, workers = 1L) {
  # ロゴ（ASCII だけ。© も ASCII 外でログ収集時に文字化けしうるので (C) と書く）
  note_Curie(r"(
  ==================================================================
       ____ _   _ ____  ___ _____
      / ___| | | |  _ \|_ _| ____|
     | |   | | | | |_) || ||  _|
     | |___| |_| |  _ < | || |___
      \____|\___/|_| \_\___|_____|    O T C   D e r i v a t i v e
  ==================================================================
    Copyright (C) Daiwa Asset Management Co.Ltd. All Rights Reserved.
)")
  note_Curie("[*]  CURIE OTC Derivative is monitoring contract PDFs.")
  note_Curie("[*]  Aladdin API payloads are prepared for each case (DRAFT / NOT SENT).")
  cores <- c(physical = parallel::detectCores(logical = FALSE), logical = parallel::detectCores(logical = TRUE))
  note_Curie("[*]  Workers: ", workers, if (workers > 1L) " (parallel, future)" else " (sequential)",
             ". CPU cores on this PC: ", cores[["physical"]], " physical / ", cores[["logical"]], " logical.")
  note_path_Curie("Watching PDF files directly under this folder (subfolders such as processed are excluded).", config$inbox, "dir")
  note_path_Curie("Completed cases are saved under this folder.", config$processed, "dir")
  if (length(extra)) note_Curie(paste(extra, collapse = "\n"))
}

# 監視フォルダー直下の PDF の一覧（サイズ・更新時刻と、その状態を最初に見た時刻）。サブフォルダー（processed など）は含めない
.inbox_snapshot_c1 <- function(inbox, previous = NULL, now = Sys.time()) {
  files <- list.files(inbox, pattern = "\\.pdf$", ignore.case = TRUE, full.names = TRUE, recursive = FALSE)
  info <- file.info(files)
  prev <- previous %||% tibble::tibble(path = character(), size = numeric(), mtime = numeric(), since = numeric())
  tibble::tibble(path = files, size = as.numeric(info$size), mtime = as.numeric(info$mtime)) |>
    dplyr::filter(!info$isdir) |>
    dplyr::left_join(dplyr::select(prev, "path", "size", "mtime", "since"), by = c("path", "size", "mtime")) |>
    dplyr::mutate(since = dplyr::coalesce(.data$since, as.numeric(now)))
}

# 監視フォルダーを1回スキャンし、サイズと更新時刻が stable_sec 秒以上変わっていない PDF を purrr::walk() で1件ずつ処理する。
# snapshot    ：前回のスキャンの一覧（監視から呼ぶとき）。NULL なら一覧を取り、stable_sec 秒待ってから確かめる
# retry_failed：前回失敗した PDF（同じ SHA-256）も処理する。既定は FALSE（失敗した PDF で AI を呼び続けない）
# .walk       ：反復の関数。並列にするときは furrr::future_walk に替える（各ワーカーで Curie を読み込んでおく）
# banner      ：最初に起動メッセージ（監視フォルダーと保存先）を表示する（監視からはスキャンごとには出さない）
scan_c1_inbox_once <- function(config, ..., snapshot = NULL, retry_failed = FALSE, banner = TRUE, .walk = purrr::walk) {
  if (banner) c1_inbox_banner(config, workers = .workers_c1(.walk))
  # 読み方が使えなければ、PDF に触る前に止める（配布用 API が未設定なら英語で理由を出す。Claude Code へは切り替えない）
  check_reader_c1(match.arg(list(...)$claude_mode %||% .reader_modes_c1[1], .reader_modes_c1))
  scan_id <- paste0("scan_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", paste(as.character(openssl::rand_bytes(2)), collapse = ""))
  seen <- snapshot$path %||% character()   # 前回のスキャンで見た PDF（新しく現れた PDF を「検出」として知らせる）
  if (is.null(snapshot)) {
    snapshot <- .inbox_snapshot_c1(config$inbox)
    if (nrow(snapshot) && config$stable_sec > 0) Sys.sleep(config$stable_sec)
  }
  now <- Sys.time()
  snap <- .inbox_snapshot_c1(config$inbox, snapshot, now)
  ready <- dplyr::filter(snap, as.numeric(now) - .data$since >= config$stable_sec)
  purrr::walk(setdiff(snap$path, seen), \(p) .c1_event(config, "detected", basename(p), scan_id, path = p,
                                                        reason = if (p %in% ready$path) "" else "waiting until the copy is complete"))
  # 最後の状態が「失敗」の内容は保留する
  history <- read_c1_history(config)
  failed_sha <- if (retry_failed) character() else history |>
    dplyr::slice_tail(n = 1, by = "sha256") |> dplyr::filter(.data$status == "失敗") |> dplyr::pull("sha256")
  ready <- dplyr::mutate(ready, sha256 = purrr::map_chr(.data$path, \(p) if (file.exists(p)) pdf_sha256_c1(p) else ""))
  held <- dplyr::filter(ready, .data$sha256 %in% failed_sha)
  todo <- dplyr::filter(ready, !.data$sha256 %in% failed_sha, nzchar(.data$sha256))

  .walk(todo$path, \(p) process_c1_pdf(p, config, ..., scan_id = scan_id))

  results <- read_c1_history(config) |> dplyr::filter(.data$scan_id == .env$scan_id)
  events <- purrr::keep(.read_c1_events(config), \(ev) identical(ev$scan_id, scan_id))
  out <- list(scan_id = scan_id, results = results, events = events, skipped = setdiff(basename(todo$path), results$original_name),
              held = basename(held$path), waiting = basename(setdiff(snap$path, ready$path)),
              snapshot = dplyr::filter(snap, file.exists(.data$path)))
  class(out) <- "c1_inbox_scan"
  invisible(out)
}

# 履歴の状態（ファイルに残す値）→ コンソール表示用の英語
.status_en_c1 <- function(status) {
  en <- c(保存済み = "Processing completed", 要確認 = "Saved for review", 重複 = "Duplicate skipped",
          失敗 = "Processing failed", スキップ = "Skipped")
  unname(dplyr::coalesce(en[status], status))
}

print.c1_inbox_scan <- function(x, ...) {
  if (!length(x$events) && !nrow(x$results) && !length(x$skipped) && !length(x$waiting) && !length(x$held)) {
    note_Curie("No PDF files to process.")
    return(invisible(x))
  }
  purrr::walk(x$events, .say_c1_event)
  purrr::walk(x$skipped, \(f) note_Curie("Skipped. ", f, " (being processed; checked again on the next scan)"))
  purrr::walk(x$held, \(f) note_Curie("On hold. ", f, " (failed previously; see failed/. Use retry_failed = TRUE to retry)"))
  purrr::walk(x$waiting, \(f) note_Curie("Waiting. ", f, " (size or modified time still changing; may be copying)"))
  invisible(x)
}

# ---- 保存済みの抽出結果からの作り直し（AI を呼ばない） -----------------------------------------------
# 案件フォルダーの extraction/（Claude の reviewed.json・fpml.xml・review_items.md）と PDF から、検証・FpML・TSV・payload・
# 共通名を作り直す。自社の名前（our_party_names）などの対応表を設定した後に、要確認の案件を保存し直すときに使う。
# processed／review の案件は、元のフォルダーを一時的に退避し、新しい保存先に保存できてから消す（失敗すれば元に戻す）。
# output/c1_trs/cases/<案件ID>/ の従来の案件フォルダー（agents/claude/ と source/）も入力にできる（元のフォルダーは消さない）
regenerate_c1_case <- function(case_dir, config, aladdin_map = read_aladdin_map_c1(), xsd_path = Sys.getenv("Curie_FPML_XSD", ""),
                               keep_review = TRUE) {
  case_dir <- path_display_Curie(case_dir)
  extraction <- purrr::detect(file.path(case_dir, c("extraction", "agents/claude")), \(d) file.exists(file.path(d, "reviewed.json")))
  if (is.null(extraction)) stop("No saved extraction results (reviewed.json) in the case folder.\n", path_line_Curie(case_dir, "dir"), call. = FALSE)
  pdf <- c(list.files(case_dir, pattern = "\\.pdf$", full.names = TRUE),
           list.files(file.path(case_dir, "source"), pattern = "^original\\.(pdf|htm)$", full.names = TRUE))[1]
  if (is.na(pdf)) stop("No PDF in the case folder.\n", path_line_Curie(case_dir, "dir"), call. = FALSE)
  old <- if (file.exists(file.path(case_dir, "validation.json"))) jsonlite::read_json(file.path(case_dir, "validation.json")) else list()
  original <- old$original_name %||% basename(pdf)
  in_box <- any(startsWith(case_dir, paste0(c(config$processed, config$review), "/")))

  job_id <- paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_", paste(as.character(openssl::rand_bytes(4)), collapse = ""))
  job_dir <- file.path(config$work, job_id)
  dir.create(job_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(job_dir, recursive = TRUE), add = TRUE)
  case <- workflows_c1_case(pdf, "saved", case_id = "case", output_root = job_dir, claude_demo_dir = extraction,
                            xsd_path = xsd_path, instruction_path = config$instruction_path, aladdin_map = aladdin_map)
  f <- case$results$failure
  if (!is.null(f)) stop(.failure_kind_en_c1(f$kind), ". ", f$message, call. = FALSE)
  naming <- c1_common_name(case$validation$conditions, aladdin_map)
  # 要確認のまま（名前が決まらない）なら、review の案件はそのままにする
  if (is.na(naming$name) && keep_review && startsWith(case_dir, paste0(config$review, "/"))) {
    note_path_Curie(paste0("Still needs review. ", original, " (", paste(unlist(naming$missing), collapse = "; "), ")"), case_dir, "dir")
    return(invisible(NULL))
  }

  # 元のフォルダーを退避してから保存する（同じ名前を使えるように。上書きはしない）
  parked <- if (in_box) paste0(case_dir, ".regenerating") else NULL
  if (in_box && !file.rename(case_dir, parked)) stop("Cannot move the case folder aside.\n", path_line_Curie(case_dir, "dir"), call. = FALSE)
  pdf_now <- if (in_box) file.path(parked, basename(pdf)) else pdf
  saved <- tryCatch(.save_c1_case(case, config, naming, job_dir,
                                  list(pdf = pdf_now, original = original, sha256 = pdf_sha256_c1(pdf_now), job_id = job_id)),
                    error = function(e) {
                      if (in_box) file.rename(parked, case_dir)
                      stop(e)
                    })
  if (in_box) unlink(parked, recursive = TRUE)
  .append_history_c1(config, list(time = .now_c1(), scan_id = "regenerate", job_id = job_id, original_name = original,
                                  sha256 = pdf_sha256_c1(file.path(saved$dest, paste0(saved$name, ".pdf"))), status = saved$status,
                                  saved_name = saved$name, saved_dir = saved$dest,
                                  reason = paste0("regenerated from saved extraction (AI not called); previous: ", basename(case_dir))))
  say_path_Curie(paste0(.status_en_c1(saved$status), ". ", original, " (regenerated from saved extraction; AI not called)"), saved$dest, "dir")
  invisible(saved)
}

# review の案件をすべて作り直す（自社の名前を設定した後など。AI は呼ばない）。1件の失敗で止めない
regenerate_c1_review_cases <- function(config, aladdin_map = read_aladdin_map_c1(), ...) {
  dirs <- list.dirs(config$review, recursive = FALSE)
  if (!length(dirs)) note_Curie("No cases to regenerate.")
  purrr::walk(dirs, \(d) tryCatch(regenerate_c1_case(d, config, aladdin_map, ...),
                                  error = function(e) note_Curie("Regeneration failed. ", basename(d), " (", conditionMessage(e), ")")))
  invisible(read_c1_history(config) |> dplyr::filter(.data$scan_id == "regenerate"))
}

# ---- 監視（一定間隔でスキャンする。別の R プロセスで動かすので、RStudio のコンソールは使える） ----------------
.watch_paths_c1 <- function(config) {
  d <- file.path(config$root, "watch")
  dir.create(d, showWarnings = FALSE)
  list(dir = d, settings = file.path(d, "settings.json"), stop = file.path(d, "STOP"), lock = file.path(d, "running"),
       heartbeat = file.path(d, "heartbeat"), log = file.path(config$logs, "watch.log"))
}
.c1_watch_env <- new.env(parent = emptyenv())

# 動いている監視プロセス（コマンドラインに監視スクリプトとこのフォルダーの settings.json があるもの）。
# 起動したセッションでなくても（R を再起動した後でも）見つけられる
.watch_workers_c1 <- function(w) {
  target <- tolower(path_display_Curie(w$settings))
  procs <- dplyr::filter(ps::ps(), tolower(.data$name) == "rscript.exe")
  hit <- purrr::map_lgl(procs$ps_handle, \(h) tryCatch({
    cmd <- tolower(gsub("\\\\", "/", ps::ps_cmdline(h)))
    any(grepl("c1_inbox_watch_worker.r", cmd, fixed = TRUE)) && any(grepl(target, cmd, fixed = TRUE))
  }, error = function(e) FALSE))
  procs$ps_handle[hit]
}

# 監視を始める。interval_sec ごとに scan_c1_inbox_once() を呼ぶ（最長 max_hours 時間）。
# 処理中の PDF があるときは、その1件が終わってから止まる
# follow = TRUE なら、出来事をこの R セッション（RStudio のコンソール）に表示する（follow_c1_inbox_watch）
start_c1_inbox_watch <- function(config, interval_sec = 30, claude_mode = .reader_modes_c1, claude_demo_dir = NULL, max_hours = 24,
                                 follow = TRUE) {
  claude_mode <- match.arg(claude_mode)
  check_reader_c1(claude_mode)   # 配布用 API が未設定なら、監視を始めずに英語で理由を出す
  w <- .watch_paths_c1(config)
  # 動いている印が残っていても、監視プロセスがなければ古い印として消す
  if (dir.exists(w$lock) && !length(.watch_workers_c1(w))) {
    unlink(c(w$lock, w$stop), recursive = TRUE)
    note_Curie("[i]  Removed a stale watcher lock (no watcher process was running).")
  }
  if (!dir.create(w$lock, showWarnings = FALSE)) {
    stop("The watcher is already running. Stop it with stop_c1_inbox_watch(config).\n", path_line_Curie(w$lock, "dir"), call. = FALSE)
  }
  unlink(w$stop)
  jsonlite::write_json(list(root = config$root, inbox = config$inbox, processed = config$processed, review = config$review,
                            failed = config$failed, stable_sec = config$stable_sec, project_root = config$project_root,
                            interval_sec = interval_sec, claude_mode = claude_mode, claude_demo_dir = claude_demo_dir %||% "",
                            max_hours = max_hours), w$settings, auto_unbox = TRUE, pretty = TRUE)
  p <- processx::process$new(file.path(R.home("bin"), "Rscript.exe"),
                             c(file.path(config$project_root, "scripts", "c1_inbox_watch_worker.R"), w$settings),
                             wd = config$project_root, stdout = w$log, stderr = "2>&1", cleanup = FALSE)
  assign(config$root, p, envir = .c1_watch_env)
  c1_inbox_banner(config)
  note_Curie("[*]  Watcher started. Scan interval: ", interval_sec, " s (separate R process).")
  note_path_Curie("Watcher log.", w$log, "file")
  if (follow) follow_c1_inbox_watch(config)
  invisible(p)
}

.c1_follow_env <- new.env(parent = emptyenv())

# 監視の出来事（PDF の検出・処理開始・完了・要確認・重複・失敗）を、この R セッション（RStudio のコンソール）に表示する。
# later で interval_sec 秒ごとに events.jsonl の新しい行を読む（コンソールは使える）。監視が終わると表示も止まる。
# R を再起動した後や、別のセッションから見るときは follow_c1_inbox_watch(config) を呼ぶ
follow_c1_inbox_watch <- function(config, interval_sec = 2) {
  w <- .watch_paths_c1(config)
  state <- new.env()
  state$n <- length(.read_c1_events(config))   # これまでの出来事は表示しない
  assign(config$root, state, envir = .c1_follow_env)   # 呼び直すと、前の表示は止まる
  tick <- function() {
    if (!identical(get0(config$root, envir = .c1_follow_env), state)) return(invisible())
    events <- .read_c1_events(config)
    if (length(events) > state$n) {
      purrr::walk(events[(state$n + 1L):length(events)], .say_c1_event)
      state$n <- length(events)
    }
    if (!dir.exists(w$lock)) {
      note_Curie(format(Sys.time(), "%H:%M:%S"), " Watcher finished.")
      rm(list = config$root, envir = .c1_follow_env)
      return(invisible())
    }
    later::later(tick, interval_sec)
  }
  later::later(tick, interval_sec)
  note_Curie("Watcher events (PDF detected, processing started, completed, failed) are shown in this console.")
  invisible(TRUE)
}

# 監視を止める（停止の印を置く。処理中の1件が終わり次第止まる）。force = TRUE ならすぐに止める
# （処理中だった PDF は元の場所に残り、ロックを外すので次のスキャンで処理し直す）
# 監視プロセスはコマンドラインで探すので、R を再起動した後のセッションからでも止められる
stop_c1_inbox_watch <- function(config, force = FALSE) {
  w <- .watch_paths_c1(config)
  workers <- .watch_workers_c1(w)
  clear <- function() unlink(c(w$lock, w$stop, list.files(config$locks, full.names = TRUE), list.files(config$work, full.names = TRUE)),
                             recursive = TRUE)
  if (!length(workers)) {
    clear()
    note_Curie("The watcher was not running. Watcher lock cleared.")
  } else if (force) {
    purrr::walk(workers, \(h) tryCatch(ps::ps_kill(h), error = function(e) NULL))
    still <- length(.watch_workers_c1(w))
    if (!still) clear()
    note_Curie(if (still) "Could not stop the watcher process. Try again." else "Watcher stopped.")
  } else {
    writeLines(.now_c1(), w$stop)
    note_Curie("Stop requested. The watcher stops before the next scan (after the PDF in progress, if any).")
  }
  invisible(TRUE)
}

c1_inbox_watch_status <- function(config) {
  w <- .watch_paths_c1(config)
  list(running = dir.exists(w$lock), alive = length(.watch_workers_c1(w)) > 0,
       stop_requested = file.exists(w$stop),
       last_scan = if (file.exists(w$heartbeat)) readLines(w$heartbeat, warn = FALSE)[1] else NA_character_, log = w$log)
}
