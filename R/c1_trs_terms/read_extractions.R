# c1：AI（Copilot／Claude）が出力した抽出JSONの読込と形式チェック（jsonlite）。
#
# 抽出JSONの形（docs/c1_trs_extraction_instructions.md と同じ）
# {
#   "document": {"document_id": "...", "file": "...", "dealer": "...", "pages": 2, "pdf_sha256": "...",
#                "extracted_by": "claude", "model": "...", "instruction_version": "c1-extract-v1",
#                "extracted_at": "2026-10-26T10:00:00+09:00"},
#   "fields": [
#     {"field": "funding.floating_rate_index", "value": "USD-LIBOR-BBA", "value_status": "stated",
#      "page": 5, "quote": "USD-LIBOR-BBA", "confidence": "high", "note": ""}
#   ]
# }
# value_status：stated／blank／bracketed／not_found（R/c1_trs_terms/trs_fields.R）。
# stated 以外は value を null にし、原文の空欄・選択肢は quote に残す。推測で埋めない。

.extraction_confidence_levels_c1 <- c("high", "medium", "low")

.as_chr_c1 <- function(v) if (is.null(v) || (length(v) == 1L && is.na(v))) NA_character_ else as.character(v)

# 1ファイルを読み、項目ごとの tibble（1項目1行）にする
read_trs_extraction_c1 <- function(path, dictionary = trs_field_dictionary_c1()) {
  if (!file.exists(path)) stop("抽出JSONがありません: ", path, call. = FALSE)
  x <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  doc <- x$document
  needed <- c("document_id", "file", "extracted_by")
  missing <- needed[purrr::map_lgl(needed, \(k) is.null(doc[[k]]) || !nzchar(doc[[k]]))]
  if (length(missing)) stop(basename(path), "：document に必須項目がありません: ", paste(missing, collapse = ", "), call. = FALSE)
  fields <- purrr::map(x$fields, function(f) {
    tibble::tibble(
      field = .as_chr_c1(f$field), value = .as_chr_c1(f$value),
      value_status = tolower(.as_chr_c1(f$value_status)),
      page = suppressWarnings(as.integer(.as_chr_c1(f$page))),
      quote = .as_chr_c1(f$quote), confidence = tolower(.as_chr_c1(f$confidence)), note = .as_chr_c1(f$note)
    )
  }) |> purrr::list_rbind()
  if (!nrow(fields)) {
    fields <- tibble::tibble(field = character(), value = character(), value_status = character(), page = integer(),
                             quote = character(), confidence = character(), note = character())
  }
  fields <- fields |>
    dplyr::mutate(
      value = dplyr::if_else(!is.na(.data$value) & !nzchar(trimws(.data$value)), NA_character_, .data$value),
      # 状態の書き忘れは、値の有無から補う（値があれば stated、なければ not_found）
      value_status = dplyr::coalesce(.data$value_status, dplyr::if_else(is.na(.data$value), "not_found", "stated"))
    )
  # 形式の問題（読み込みは止めず、記録して確認表に出す）
  unknown <- setdiff(fields$field, dictionary$field)
  dup <- unique(fields$field[duplicated(fields$field)])
  bad_status <- unique(fields$value_status[!fields$value_status %in% .value_status_levels_c1])
  stated_without_value <- fields$field[fields$value_status == "stated" & is.na(fields$value)]
  value_without_stated <- fields$field[fields$value_status != "stated" & !is.na(fields$value)]
  issues <- c(
    if (length(unknown)) paste0("定義にない項目: ", paste(unknown, collapse = ", ")),
    if (length(dup)) paste0("同じ項目が複数: ", paste(dup, collapse = ", ")),
    if (length(bad_status)) paste0("value_status が不正: ", paste(bad_status, collapse = ", ")),
    if (length(stated_without_value)) paste0("stated なのに値がない: ", paste(stated_without_value, collapse = ", ")),
    if (length(value_without_stated)) paste0("stated 以外なのに値がある（値は使わない）: ", paste(value_without_stated, collapse = ", ")),
    if (any(!is.na(fields$confidence) & !fields$confidence %in% .extraction_confidence_levels_c1)) "confidence が high／medium／low 以外"
  )
  fields <- fields |>
    dplyr::mutate(value = dplyr::if_else(.data$value_status == "stated", .data$value, NA_character_))
  fields |>
    dplyr::filter(.data$field %in% dictionary$field, !duplicated(.data$field)) |>
    dplyr::mutate(
      document_id = .as_chr_c1(doc$document_id), file = .as_chr_c1(doc$file),
      dealer_on_cover = .as_chr_c1(doc$dealer), pages = suppressWarnings(as.integer(.as_chr_c1(doc$pages))),
      pdf_sha256 = .as_chr_c1(doc$pdf_sha256), agent = tolower(.as_chr_c1(doc$extracted_by)),
      model = .as_chr_c1(doc$model), instruction_version = .as_chr_c1(doc$instruction_version),
      extracted_at = .as_chr_c1(doc$extracted_at), extraction_file = basename(path), .before = 1
    ) |>
    structure(format_issues = issues)
}

# 複数ファイルを読む。形式の問題はファイルごとにまとめて返す
read_trs_extractions_c1 <- function(paths, dictionary = trs_field_dictionary_c1()) {
  if (!length(paths)) stop("抽出JSONを指定してください。", call. = FALSE)
  parsed <- purrr::map(paths, \(p) read_trs_extraction_c1(p, dictionary))
  issues <- purrr::map2(parsed, paths, \(x, p) {
    i <- attr(x, "format_issues")
    tibble::tibble(extraction_file = rep(basename(p), length(i)), issue = i)
  }) |> purrr::list_rbind()
  fields <- purrr::list_rbind(parsed)
  dup <- fields |> dplyr::distinct(.data$document_id, .data$agent, .data$extraction_file) |>
    dplyr::count(.data$document_id, .data$agent) |> dplyr::filter(.data$n > 1L)
  if (nrow(dup)) stop("同じ文書・同じAIの抽出JSONが複数あります: ",
                      paste(dup$document_id, dup$agent, collapse = ", "), call. = FALSE)
  list(fields = fields, format_issues = issues,
       runs = fields |> dplyr::distinct(dplyr::across(c("document_id", "file", "agent", "model", "instruction_version",
                                                        "extracted_at", "pdf_sha256", "extraction_file"))))
}
