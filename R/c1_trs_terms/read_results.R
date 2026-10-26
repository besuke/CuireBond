# c1：PowerShell（R/c1_trs_terms/PS/run_document.ps1）が作った案件フォルダの読込。
#
# 案件フォルダ（output/c1_trs/cases/<案件ID>/）
#   manifest.json            実行の記録（段階ごとの状態、終了コード、版の記録、モード、エラー）
#   source/original.pdf|htm  取得した原資料（そのまま）。acquisition.json に取得元・取得日時・HTTP・形式の判定根拠・SHA-256
#   source/reading_*         読解用データ（ページごとの本文。PDF の文字のないページは OCR）
#   agents/copilot/          Copilot の依頼文・回答・一次読解（copilot_extraction.json）・記録
#   agents/claude/           Claude の依頼文・原文照合（reviewed.json）・FpML 案・記録
#   result/conditions.csv    抽出条件（UTF-8、1項目1行）
#   result/conditions.json   同じ内容の JSON
#   result/fpml.xml          FpML（Claude の作成。R が構文・XSD・CSV との一致を検証する）
#   result/review_items.md   要確認事項
#   result/evidence.json     版の記録と Copilot の根拠（attributions）
#   logs/run_log.jsonl       段階ごとのログ
#
# CSV の列（1項目1行。反復するレッグ・日程は leg_id と seq で区別し、1つのセルにまとめない）
#   case_id 案件ID／item_id 項目ID／leg_id レッグ／seq 番号／value 値／unit 単位／value_status 原文の状態／
#   source_page 根拠ページ／source_location 根拠の位置／quote 原文引用／copilot_value Copilot の値／
#   review_status 確認状態（confirmed・corrected・unresolved・not_found）／review_note 確認の理由

.c1_condition_cols <- c("case_id", "item_id", "leg_id", "seq", "value", "unit", "value_status", "source_page",
                        "source_location", "quote", "copilot_value", "review_status", "review_note")

# 終了コードの意味（PS/common.ps1 と同じ）
c1_exit_code_label <- function(code) {
  labels <- c(`0` = "成果物あり（R の検証待ち）", `1` = "想定外のエラー", `2` = "未接続", `3` = "取得失敗（資料を取得できない）",
              `4` = "Copilot 読解失敗", `5` = "Copilot 回答形式不正", `6` = "Claude 読解失敗", `7` = "Claude 回答形式不正",
              `8` = "読解用データ作成失敗（本文抽出・OCR）")
  unname(dplyr::coalesce(labels[as.character(code)], paste0("不明な終了コード ", code)))
}

read_c1_case_results <- function(case_dir) {
  if (!dir.exists(case_dir)) stop("案件フォルダがありません: ", case_dir, call. = FALSE)
  manifest_path <- file.path(case_dir, "manifest.json")
  if (!file.exists(manifest_path)) stop("manifest.json がありません（PowerShell の処理が途中で止まった可能性）: ", case_dir, call. = FALSE)
  manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
  result_dir <- file.path(case_dir, "result")
  path_of <- function(f) { p <- file.path(result_dir, f); if (file.exists(p)) p else NA_character_ }
  files <- c(csv = path_of("conditions.csv"), json = path_of("conditions.json"), fpml = path_of("fpml.xml"),
             review = path_of("review_items.md"), evidence = path_of("evidence.json"))
  conditions <- if (!is.na(files[["csv"]])) {
    readr::read_csv(files[["csv"]], col_types = readr::cols(.default = readr::col_character()), na = character(),
                    locale = readr::locale(encoding = "UTF-8"), progress = FALSE)
  } else NULL
  conditions_json <- if (!is.na(files[["json"]])) jsonlite::fromJSON(files[["json"]], simplifyVector = FALSE) else NULL
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
