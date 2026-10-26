# c1：配置済み資料1件の処理（R が入口）。
#   1. PowerShell（R/c1_trs_terms/PS/run_document.ps1）を起動：認証、資料の取得と版の記録、
#      Copilot の一次読解、Claude の原文照合・FpML 作成、CSV・JSON・FpML の保存
#   2. R：終了コードと成果物を受け取り、検証（条件・根拠・版・FpML）、Excel 出力
# R は資料をアップロードしない。共有URLは利用者が事前に両AIから読める場所へ配置した資料のもの。
# 認証情報・トークンは PowerShell の中だけで扱い、R には渡らない。
#
# copilot_mode：api（実際に呼ぶ）／manual（手動回答を取り込む）／demo（モック）。黙って切り替えない
# claude_mode ：api（Claude Code を実行）／demo（用意した結果を使う）

.c1_ps_dir <- file.path("R", "c1_trs_terms", "PS")

workflows_c1_case <- function(shared_url, copilot_mode = c("api", "manual", "demo"), claude_mode = c("api", "demo"),
                              case_id = NULL, output_root = file.path("output", "c1_trs", "cases"),
                              copilot_file = NULL, claude_demo_dir = NULL,
                              xsd_path = Sys.getenv("CURIE_FPML_XSD", ""), pwsh = Sys.which("pwsh")) {
  copilot_mode <- match.arg(copilot_mode)
  claude_mode <- match.arg(claude_mode)
  if (!nzchar(pwsh)) stop("PowerShell 7（pwsh）が見つかりません。", call. = FALSE)
  if (copilot_mode %in% c("manual", "demo") && (is.null(copilot_file) || !file.exists(copilot_file))) {
    stop("Copilot の ", copilot_mode, " モードには回答ファイル（copilot_file）が必要です。", call. = FALSE)
  }
  if (is.null(case_id)) {
    stem <- gsub("[^A-Za-z0-9_-]", "_", tools::file_path_sans_ext(basename(utils::URLdecode(sub("[?#].*$", "", shared_url)))))
    case_id <- paste0(if (nzchar(stem)) stem else "document", "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  }
  q <- function(x) paste0('"', x, '"')   # Windows のコマンドラインは二重引用符
  args <- c("-NoProfile", "-File", q(normalizePath(file.path(.c1_ps_dir, "run_document.ps1"))),
            "-SharedUrl", q(shared_url), "-CopilotMode", copilot_mode, "-ClaudeMode", claude_mode,
            "-CaseId", q(case_id), "-OutputRoot", q(normalizePath(output_root, mustWork = FALSE)),
            if (copilot_mode == "manual") c("-CopilotManualPath", q(normalizePath(copilot_file))),
            if (copilot_mode == "demo") c("-CopilotDemoPath", q(normalizePath(copilot_file))),
            if (claude_mode == "demo") c("-ClaudeDemoDir", q(normalizePath(claude_demo_dir))))
  message(format(Sys.time(), "%H:%M:%S"), " [c1] PowerShell を起動します（Copilot：", copilot_mode, "、Claude：", claude_mode, "）")
  exit <- system2(pwsh, args, stdout = "", stderr = "")
  case_dir <- file.path(output_root, case_id)
  message(format(Sys.time(), "%H:%M:%S"), " [c1] PowerShell が終了しました（終了コード ", exit, "：", c1_exit_code_label(exit), "）")
  if (!file.exists(file.path(case_dir, "manifest.json"))) {
    return(list(state = "失敗", exit_code = exit, case_dir = case_dir, error = "PowerShell が manifest.json を作らなかった"))
  }
  results <- read_c1_case_results(case_dir)
  validation <- validate_c1_case(results, xsd_path)
  ok <- identical(as.integer(exit), 0L) && !is.null(results$conditions)
  excel <- if (ok) write_c1_case_excel(results, validation) else NA_character_
  # FpML：検証済みなら最終版、それ以外（未検証・条件不足・Claude単独処理）はドラフトとして保存する
  fpml_out <- NA_character_
  if (ok && !is.na(results$files[["fpml"]])) {
    fpml_out <- file.path(case_dir, "result", if (isTRUE(validation$final)) "fpml_final.xml" else "fpml_DRAFT.xml")
    file.copy(results$files[["fpml"]], fpml_out, overwrite = TRUE)
  }
  jsonlite::write_json(list(case_id = case_id, state = validation$state, reading = validation$reading %||% "",
                            exit_code = exit, modes = as.list(validation$modes),
                            validated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"), excel = excel,
                            fpml = if (is.na(fpml_out)) NULL else basename(fpml_out), checks = validation$checks),
                       file.path(case_dir, "r_validation.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")
  list(state = validation$state, reading = validation$reading, exit_code = exit, case_dir = results$case_dir,
       results = results, validation = validation, excel = excel, fpml = fpml_out)
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
                     Copilotの値 = .data$copilot_value, 理由 = .data$review_note)
}
