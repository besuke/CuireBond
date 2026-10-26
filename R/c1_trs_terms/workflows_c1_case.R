# c1：TRS 確認書1件の処理（R が入口）。
#   1. PowerShell（R/c1_trs_terms/PS/run_document.ps1）を起動：資料の取得と記録、読解用データ、
#      Claude の読解・FpML 作成、conditions.json・FpML の保存
#   2. R：終了コードと成果物を受け取り、検証（条件・根拠・版・FpML）
# 成果物は JSON（conditions.json）と FpML だけ。FpML は検証済みなら fpml_final.xml、それ以外は fpml_DRAFT.xml。
#
# source     ：公開 PDF・HTML の URL、またはローカルのファイル（Box Drive の同期フォルダのファイルも可）
# claude_mode：api（Claude Code を実行）／demo（用意した結果を使う。Claude は呼ばない）

.c1_ps_dir <- file.path("R", "c1_trs_terms", "PS")

# コンソールに表示する主要項目（日付・参照レグ・参照金利・カレンダー。金額の項目は含めない）
.key_fields_c1 <- c(
  "trade.product_type",
  "trade.trade_date", "trade.effective_date", "trade.termination_date", "return.final_valuation_date",
  "return.underlyer_type", "return.underlyer_name", "return.underlyer_id", "return.underlyer_issuer", "return.return_type",
  "funding.floating_rate_index", "funding.index_tenor", "funding.reset_relative_to", "funding.fixing_offset_days",
  "return.valuation_calendar", "funding.fixing_calendar", "funding.payment_calendar",
  "funding.fixing_business_day_convention", "funding.payment_business_day_convention"
)

workflows_c1_case <- function(source, claude_mode = c("api", "demo"),
                              case_id = NULL, output_root = file.path("output", "c1_trs", "cases"),
                              claude_demo_dir = NULL,
                              xsd_path = Sys.getenv("CURIE_FPML_XSD", ""), pwsh = Sys.which("pwsh")) {
  claude_mode <- match.arg(claude_mode)
  if (!nzchar(pwsh)) stop("PowerShell 7（pwsh）が見つかりません。", call. = FALSE)
  if (is.null(case_id)) {
    stem <- gsub("[^A-Za-z0-9_-]", "_", tools::file_path_sans_ext(basename(utils::URLdecode(sub("[?#].*$", "", source)))))
    case_id <- paste0(if (nzchar(stem)) stem else "document", "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  }
  q <- function(x) paste0('"', x, '"')   # Windows のコマンドラインは二重引用符
  args <- c("-NoProfile", "-File", q(normalizePath(file.path(.c1_ps_dir, "run_document.ps1"))),
            "-Source", q(source), "-ClaudeMode", claude_mode,
            "-CaseId", q(case_id), "-OutputRoot", q(normalizePath(output_root, mustWork = FALSE)),
            if (claude_mode == "demo") c("-ClaudeDemoDir", q(normalizePath(claude_demo_dir))))
  message(format(Sys.time(), "%H:%M:%S"), " [c1] PowerShell を起動します（Claude：", claude_mode, "）")
  exit <- system2(pwsh, args, stdout = "", stderr = "")
  case_dir <- file.path(output_root, case_id)
  message(format(Sys.time(), "%H:%M:%S"), " [c1] PowerShell が終了しました（終了コード ", exit, "：", c1_exit_code_label(exit), "）")
  if (!file.exists(file.path(case_dir, "manifest.json"))) {
    return(list(state = "失敗", exit_code = exit, case_dir = case_dir, error = "PowerShell が manifest.json を作らなかった"))
  }
  results <- read_c1_case_results(case_dir)
  validation <- validate_c1_case(results, xsd_path)
  ok <- identical(as.integer(exit), 0L) && !is.null(results$conditions)
  # FpML：検証済みなら最終版、それ以外（未検証・条件不足）はドラフトとして保存する
  fpml_out <- NA_character_
  if (ok && !is.na(results$files[["fpml"]])) {
    fpml_out <- file.path(case_dir, "result", if (isTRUE(validation$final)) "fpml_final.xml" else "fpml_DRAFT.xml")
    file.copy(results$files[["fpml"]], fpml_out, overwrite = TRUE)
  }
  jsonlite::write_json(list(case_id = case_id, state = validation$state, exit_code = exit, modes = as.list(validation$modes),
                            validated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
                            fpml = if (is.na(fpml_out)) NULL else basename(fpml_out), checks = validation$checks),
                       file.path(case_dir, "r_validation.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")
  list(state = validation$state, exit_code = exit, case_dir = results$case_dir,
       results = results, validation = validation, fpml = fpml_out)
}

# コンソール表示用：主要項目の取得できたもの／できなかったもの
summarize_c1_case <- function(case, fields = .key_fields_c1) {
  cond <- case$validation$conditions
  if (is.null(cond)) return(NULL)
  cond |>
    dplyr::filter(.data$item_id %in% fields) |>
    dplyr::arrange(match(.data$item_id, fields), .data$seq) |>
    dplyr::transmute(区分 = .data$section, 項目 = .data$label_ja, 番号 = .data$seq,
                     結果 = dplyr::if_else(.data$value_status == "stated" & nzchar(.data$value), "取得できた", "取得できなかった"),
                     値 = .data$value, ページ = .data$source_page, 確認状態 = .data$review_status,
                     理由 = .data$review_note)
}
