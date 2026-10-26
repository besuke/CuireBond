# c1：TRS 確認書1件の処理（R → Claude Code → R）。
#   1. 取得：原資料を保存し、取得元・取得日時・HTTP・形式・文字コード・SHA-256 を記録（acquire_document.R）
#   2. 読解用データ：ページごとの本文（文字のないページは「本文なし」と記録）。引用の照合に使う（reading_data.R）
#   3. Claude Code：作業フォルダの原資料を読み、条件と FpML を作る（claude_agent.R）
#   4. 保存：result/conditions.json と FpML
#   5. 検証：条件・根拠・版・FpML（XML構文／公式XSD／条件との整合）。検証済みなら fpml_final.xml、それ以外は fpml_DRAFT.xml
#
# source     ：公開 PDF・HTML の URL、またはローカルのファイル（Box Drive の同期フォルダのファイルも可）
# claude_mode：api（Claude Code を実行）／demo（claude_demo_dir の結果を使う。Claude は呼ばない）
# 段階の状態：取得失敗／読解用データ作成失敗／読解失敗（Claude の失敗・タイムアウト・読めなかった）／回答形式不正／要確認／検証済み

# コンソールに表示する主要項目（日付・参照レグ・参照金利・カレンダー。金額の項目は含めない）
.key_fields_c1 <- c(
  "trade.product_type",
  "trade.trade_date", "trade.effective_date", "trade.termination_date", "return.final_valuation_date",
  "return.underlyer_type", "return.underlyer_name", "return.underlyer_id", "return.underlyer_issuer", "return.return_type",
  "funding.floating_rate_index", "funding.index_tenor", "funding.reset_relative_to", "funding.fixing_offset_days",
  "return.valuation_calendar", "funding.fixing_calendar", "funding.payment_calendar",
  "funding.fixing_business_day_convention", "funding.payment_business_day_convention"
)

.now_c1 <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")

# 段階の失敗（理由つき）
.stage_fail_c1 <- function(kind, message) {
  stop(structure(class = c("c1_stage_error", "error", "condition"), list(message = message, kind = kind, call = NULL)))
}

# Claude の結果（reviewed.json）→ 条件の行（1項目1件。すべて文字列）
.conditions_rows_c1 <- function(reviewed, case_id) {
  leg_of <- function(f) dplyr::case_when(startsWith(f, "return.") ~ "return_leg", startsWith(f, "funding.") ~ "interest_leg",
                                         startsWith(f, "schedule.") ~ "schedule", .default = "trade")
  s <- function(v) if (is.null(v) || !length(v)) "" else paste(as.character(unlist(v)), collapse = " / ")
  purrr::map(reviewed$fields, \(f) list(
    case_id = case_id, item_id = s(f$field), leg_id = if (nzchar(s(f$leg_id))) s(f$leg_id) else leg_of(s(f$field)),
    seq = if (nzchar(s(f$seq))) as.integer(s(f$seq)) else 1L, value = s(f$value), unit = s(f$unit),
    value_status = s(f$value_status), source_page = s(f$page), source_location = s(f$location), quote = s(f$quote),
    review_status = s(f$review_status), review_note = s(f$review_note)))
}

workflows_c1_case <- function(source, claude_mode = c("api", "demo"),
                              case_id = NULL, output_root = file.path("output", "c1_trs", "cases"),
                              claude_demo_dir = NULL, xsd_path = Sys.getenv("Curie_FPML_XSD", ""),
                              instruction_path = file.path("docs", "c1_trs_extraction_instructions.md"),
                              timeout_sec = as.numeric(Sys.getenv("Curie_CLAUDE_TIMEOUT_SEC", "1800")),
                              claude_cmd = claude_command_c1(), claude_args = NULL, fetcher = .curl_fetch_c1,
                              aladdin_map = read_aladdin_map_c1()) {
  claude_mode <- match.arg(claude_mode)
  if (is.null(case_id)) {
    stem <- gsub("[^A-Za-z0-9_-]", "_", tools::file_path_sans_ext(basename(utils::URLdecode(sub("[?#].*$", "", source)))))
    case_id <- paste0(if (nzchar(stem)) stem else "document", "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  }
  case_dir <- file.path(output_root, case_id)
  if (dir.exists(case_dir)) unlink(case_dir, recursive = TRUE)   # 同じ案件IDの古い成果物を残さない
  purrr::walk(c("source", "agents", "result", "logs"), \(d) dir.create(file.path(case_dir, d), recursive = TRUE, showWarnings = FALSE))
  log_path <- file.path(case_dir, "logs", "run_log.jsonl")
  step <- function(stage, msg, level = "info") {
    message(format(Sys.time(), "%H:%M:%S"), " [c1] ", stage, "：", msg)
    cat(jsonlite::toJSON(list(time = .now_c1(), stage = stage, level = level, message = msg), auto_unbox = TRUE), "\n",
        file = log_path, append = TRUE, sep = "")
  }
  manifest <- list(case_id = case_id, created_at = .now_c1(), claude_mode = claude_mode,
                   stages = list(document = "未実行", reading_data = "未実行", claude = "未実行", outputs = "未実行"))
  failure <- tryCatch({
    # 1. 取得 ---------------------------------------------------------------------------------
    step("資料の取得", source)
    # 処理は1つにまとめる（tryCatch に2つ並べると、1つ目の中で出したエラーを2つ目が受け取ってしまうため）
    acq <- tryCatch(acquire_document_c1(source, file.path(case_dir, "source"), fetcher = fetcher),
                    error = function(e) .stage_fail_c1("取得失敗", if (inherits(e, "c1_acquire_error")) conditionMessage(e) else
                      paste0("資料を取得できません（取得失敗）: ", conditionMessage(e))))
    manifest$source <- acq$source; manifest$source_type <- acq$source_type
    manifest$acquisition <- acq[setdiff(names(acq), "path")]
    jsonlite::write_json(manifest$acquisition, file.path(case_dir, "source", "acquisition.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")
    manifest$stages$document <- "取得済み"

    # 2. 読解用データ ------------------------------------------------------------------------
    # 文字のないページ（スキャン画像など）があっても止めない（Claude は原資料を画像として読める。引用は照合できない）
    pages <- tryCatch(document_pages_c1(acq$path, acq$format$kind),
                      error = function(e) .stage_fail_c1("読解用データ作成失敗", paste0("読解用データを作れません: ", conditionMessage(e))))
    jsonlite::write_json(pages, file.path(case_dir, "source", "reading_pages.json"), pretty = TRUE)
    methods <- pages |> dplyr::count(.data$method) |>
      dplyr::transmute(x = paste0(.data$method, " ", .data$n, " ページ")) |> dplyr::pull("x") |> paste(collapse = "、")
    manifest$reading_data <- list(pages = nrow(pages), methods = methods, text_chars = sum(pages$chars))
    manifest$stages$reading_data <- paste0("作成済み（", methods, "）")
    document <- list(document_id = tools::file_path_sans_ext(gsub("[^A-Za-z0-9_.-]", "_", acq$name)), file_name = acq$name,
                     pages = nrow(pages), sha256 = acq$sha256, kind = acq$format$kind,
                     local_name = paste0("document.", tools::file_ext(acq$path)), source_type = acq$source_type)
    manifest$document <- document
    step("資料の取得", sprintf("%s（%s、%d ページ、SHA-256 %s…、本文 %s%s）", acq$name, toupper(acq$format$kind), nrow(pages),
                          substr(acq$sha256, 1, 12), methods, if (!is.na(acq$encoding)) paste0("、文字コード ", acq$encoding) else ""))

    # 3. Claude --------------------------------------------------------------------------------
    agent_dir <- file.path(case_dir, "agents", "claude")
    step("Claude 依頼文作成", paste0("モード ", claude_mode))
    prepare_claude_workdir_c1(agent_dir, document, acq$path, instruction_path)
    run <- if (claude_mode == "demo") {
      step("Claude 実行", "デモ（用意した結果を使う。Claude は呼ばない）")
      run_claude_demo_c1(agent_dir, claude_demo_dir)
    } else {
      step("Claude 実行", "原文の読解と FpML 作成（数分かかります。この間は表示が止まります）")
      run_claude_c1(agent_dir, document, cmd = claude_cmd, args = claude_args %||% claude_args_c1(document), timeout_sec = timeout_sec)
    }
    manifest$claude <- run$log
    if (run$state == "timeout") .stage_fail_c1("読解失敗", run$log$error)
    if (run$state != "ok") .stage_fail_c1("読解失敗", run$log$error)
    if (claude_mode == "api") step("Claude 回答取得", sprintf("%s 秒、費用の目安 %s USD", run$log$seconds, format(run$log$cost_usd)))
    out <- check_claude_outputs_c1(agent_dir)
    manifest$claude$output_check <- out$state
    if (out$state %in% c("unreadable", "missing")) .stage_fail_c1("読解失敗", paste(out$issues, collapse = "; "))
    if (out$state == "invalid") .stage_fail_c1("回答形式不正", paste0("回答の形式が不正: ", paste(out$issues, collapse = "; ")))
    manifest$stages$claude <- paste0("取得済み", if (claude_mode == "demo") "（デモ：Claude は呼んでいない）" else "")

    # 4. 成果物 --------------------------------------------------------------------------------
    step("成果物の保存", file.path(case_dir, "result"))
    review_text <- paste(readLines(file.path(agent_dir, "review_items.md"), encoding = "UTF-8", warn = FALSE), collapse = "\n")
    jsonlite::write_json(list(case_id = case_id, document = document, acquisition = manifest$acquisition,
                              reading_data = manifest$reading_data, review_items = review_text,
                              conditions = .conditions_rows_c1(out$reviewed, case_id)),
                         file.path(case_dir, "result", "conditions.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")
    file.copy(file.path(agent_dir, "fpml.xml"), file.path(case_dir, "result", "fpml.xml"), overwrite = TRUE)
    manifest$stages$outputs <- "保存済み"
    NULL
  }, c1_stage_error = function(e) {
    list(kind = e$kind, message = conditionMessage(e))
  }, error = function(e) list(kind = "失敗", message = paste0("想定外のエラー: ", conditionMessage(e))))

  if (!is.null(failure)) {
    step("中止", failure$message, "error")
    manifest$failure <- failure
    if (identical(failure$kind, "取得失敗")) manifest$stages$document <- "取得失敗"
    if (identical(failure$kind, "読解用データ作成失敗")) manifest$stages$reading_data <- "作成失敗"
    if (failure$kind %in% c("読解失敗", "回答形式不正")) manifest$stages$claude <- failure$kind
  }
  manifest$finished_at <- .now_c1()
  jsonlite::write_json(manifest, file.path(case_dir, "manifest.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")

  # 5. 検証（保存したものを読み直して検証する）--------------------------------------------------
  results <- read_c1_case_results(case_dir)
  validation <- validate_c1_case(results, xsd_path)
  fpml_out <- NA_character_
  if (is.null(failure) && !is.na(results$files[["fpml"]])) {
    fpml_out <- file.path(case_dir, "result", if (isTRUE(validation$final)) "fpml_final.xml" else "fpml_DRAFT.xml")
    file.copy(results$files[["fpml"]], fpml_out, overwrite = TRUE)
  }
  jsonlite::write_json(list(case_id = case_id, state = validation$state, modes = as.list(validation$modes),
                            validated_at = .now_c1(), fpml = if (is.na(fpml_out)) NULL else basename(fpml_out),
                            verdicts = validation$verdicts, checks = validation$checks),
                       file.path(case_dir, "r_validation.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")
  # Aladdin 送信用 JSON の下書き（送信はしない。送信先・認証が未提示）
  out <- list(state = validation$state, case_dir = results$case_dir, results = results, validation = validation, fpml = fpml_out)
  out$aladdin <- if (is.null(failure)) write_aladdin_draft_c1(out, aladdin_map) else NULL
  step("終了", if (is.null(failure)) paste0("保存しました：", normalizePath(case_dir, winslash = "/")) else failure$kind)
  out
}

# コンソール表示用：主要項目の取得できたもの／できなかったもの
summarize_c1_case <- function(case, fields = .key_fields_c1) {
  cond <- case$validation$conditions
  if (is.null(cond)) return(NULL)
  cond |>
    dplyr::filter(.data$item_id %in% fields) |>
    dplyr::arrange(match(.data$item_id, fields), .data$seq) |>
    dplyr::transmute(区分 = .data$section, 項目 = .data$label_ja, レッグ = .data$leg_id, 番号 = .data$seq,
                     結果 = dplyr::if_else(.data$value_status == "stated" & nzchar(.data$value), "取得できた", "取得できなかった"),
                     値 = .data$value, ページ = .data$source_page, 引用の確認 = .data$引用の実在確認,
                     確認状態 = .data$review_status, 理由 = .data$review_note)
}
