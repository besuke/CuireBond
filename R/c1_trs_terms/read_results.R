# c1：PowerShell（R/c1_trs_terms/PS/run_document.ps1）が作った案件フォルダの読込。
#
# 案件フォルダ（output/c1_trs/cases/<案件ID>/）
#   manifest.json            実行の記録（段階ごとの状態、終了コード、モード、エラー）
#   source/original.pdf|htm  取得した原資料（そのまま）。acquisition.json に取得元・取得日時・HTTP・形式の判定根拠・SHA-256
#   source/reading_*         読解用データ（ページごとの本文。PDF の文字のないページは OCR）
#   agents/claude/           Claude への依頼文・読解結果（reviewed.json）・FpML 案・記録
#   result/conditions.json   成果物：条件（1項目1件）、資料・取得の記録、要確認事項
#   result/fpml.xml          成果物：FpML（Claude の作成。R が構文・XSD・条件との一致を検証し、
#                            fpml_final.xml または fpml_DRAFT.xml として保存する）
#   logs/run_log.jsonl       段階ごとのログ
#
# 条件の項目（1項目1件。反復するレッグ・日程は leg_id と seq で区別し、1つの値にまとめない）
#   case_id 案件ID／item_id 項目ID／leg_id レッグ／seq 番号／value 値／unit 単位／value_status 原文の状態／
#   source_page 根拠ページ／source_location 根拠の位置／quote 原文引用／
#   review_status 確認状態（confirmed・unresolved・not_found）／review_note 確認の理由

.c1_condition_cols <- c("case_id", "item_id", "leg_id", "seq", "value", "unit", "value_status", "source_page",
                        "source_location", "quote", "review_status", "review_note")

# 終了コードの意味（PS/common.ps1 と同じ）
c1_exit_code_label <- function(code) {
  labels <- c(`0` = "成果物あり（R の検証待ち）", `1` = "想定外のエラー", `3` = "取得失敗（資料を取得できない）",
              `6` = "Claude 読解失敗", `7` = "Claude 回答形式不正", `8` = "読解用データ作成失敗（本文抽出・OCR）")
  unname(dplyr::coalesce(labels[as.character(code)], paste0("不明な終了コード ", code)))
}

# conditions.json の条件を表にする（すべて文字列。値がなければ空文字）
.conditions_table_c1 <- function(rows) {
  purrr::map(rows, \(r) tibble::as_tibble(purrr::map(rlang::set_names(.c1_condition_cols), \(k) {
    v <- r[[k]]
    if (is.null(v) || !length(v)) "" else paste(as.character(unlist(v)), collapse = " / ")
  }))) |>
    purrr::list_rbind()
}

read_c1_case_results <- function(case_dir) {
  if (!dir.exists(case_dir)) stop("案件フォルダがありません: ", case_dir, call. = FALSE)
  manifest_path <- file.path(case_dir, "manifest.json")
  if (!file.exists(manifest_path)) stop("manifest.json がありません（PowerShell の処理が途中で止まった可能性）: ", case_dir, call. = FALSE)
  manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
  result_dir <- file.path(case_dir, "result")
  path_of <- function(f) { p <- file.path(result_dir, f); if (file.exists(p)) p else NA_character_ }
  files <- c(json = path_of("conditions.json"), fpml = path_of("fpml.xml"))
  conditions_json <- if (!is.na(files[["json"]])) jsonlite::fromJSON(files[["json"]], simplifyVector = FALSE) else NULL
  conditions <- if (!is.null(conditions_json) && length(conditions_json$conditions)) .conditions_table_c1(conditions_json$conditions) else NULL
  # 原資料（PDF または HTML。取得したもの）と、ページごとの読み方（本文抽出／OCR）
  original <- list.files(file.path(case_dir, "source"), pattern = "^original[.](pdf|htm)$", full.names = TRUE)
  reading_path <- file.path(case_dir, "source", "reading_pages.json")
  reading <- if (file.exists(reading_path)) {
    purrr::map(jsonlite::fromJSON(reading_path, simplifyVector = FALSE), \(p) tibble::tibble(page = as.integer(p$page), method = p$method)) |>
      purrr::list_rbind()
  } else NULL
  list(case_dir = normalizePath(case_dir, winslash = "/"), manifest = manifest, files = files,
       conditions = conditions, conditions_json = conditions_json,
       source_path = if (length(original)) original[1] else NA_character_,
       source_kind = manifest$document$kind %||% NA_character_, reading_pages = reading,
       exit_code = manifest$exit_code %||% NA_integer_, ps_state = manifest$state %||% NA_character_)
}
