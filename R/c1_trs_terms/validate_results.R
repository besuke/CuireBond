# c1：PowerShell の成果物（CSV・JSON・FpML）の検証と、確認用の表・Excel。
#
# 状態の区別
#   未接続／読解失敗／回答形式不正 … PowerShell の段階で止まった（終了コードで判断。検証はしない）
#   要確認   … 成果物はあるが、検証の不合格、または必須項目の未確定がある
#   検証済み … R の検証にすべて合格し、必須項目がすべて原文で確認された（Copilot・Claude が api のとき）
#   デモ     … Copilot または Claude がデモ（モック）のため、検証に合格しても検証済みとはしない
# これは独立した二重読解ではない（Copilot の一次読解を Claude が原文で確かめる）。「2AI独立照合済み」とは表示しない。
# FpML は「XML構文」「公式XSD」「CSV との一致」を別々に判定する。Aladdin 用JSONは仕様の提示前のため作らない。

.c1_review_levels <- c("confirmed", "corrected", "unresolved", "not_found")

# 項目ごとに期待する単位（空なら単位を問わない）
.expected_unit_c1 <- function(item_id, type, notional_currency = NA_character_) {
  dplyr::case_when(
    type == "date" ~ "date",
    item_id == "funding.spread_bp" ~ "bp",
    item_id == "funding.rate_floor_pct" ~ "percent",
    type == "integer" & grepl("_days$", item_id) ~ "business_days",
    item_id == "trade.notional_amount" ~ dplyr::coalesce(notional_currency, ""),
    .default = ""
  )
}

.check_row_c1 <- function(group, check, ok, detail = "", na_label = "未実施") {
  tibble::tibble(区分 = group, 検証 = check,
                 結果 = if (is.na(ok)) na_label else if (isTRUE(ok)) "合格" else "不合格",
                 詳細 = paste(detail, collapse = "; "))
}

# 値の型の確認（stated の値だけ）
.type_ok_c1 <- function(value, type) {
  dplyr::case_when(
    type == "date" ~ grepl("^\\d{4}-\\d{2}-\\d{2}$", value) & !is.na(suppressWarnings(as.Date(value, "%Y-%m-%d"))),
    type == "number" ~ !is.na(suppressWarnings(as.numeric(value))),
    type == "integer" ~ grepl("^-?\\d+$", value),
    .default = nzchar(value)
  )
}

# 原資料の本文（ページごと）。R が独自に抜き出す（PDF は pdftools、HTML は xml2 で全体を1ページ）
.source_pages_c1 <- function(path, kind) {
  if (identical(kind, "html")) {
    doc <- xml2::read_html(path)
    xml2::xml_remove(xml2::xml_find_all(doc, "//script|//style"))
    return(paste(xml2::xml_text(xml2::xml_find_all(doc, "//body//text()")), collapse = " "))
  }
  pdftools::pdf_text(path)
}

# 原文引用の実在確認（ページ指定。空白・引用符の違いは吸収する）
# OCR で読んだページは、原資料に文字がないため照合できない（要確認として扱う）
.quote_checks_c1 <- function(conditions, source_path, kind, reading_pages = NULL) {
  if (is.na(source_path)) return(rep("原資料なし", nrow(conditions)))
  pages <- .normalize_text_c1(.source_pages_c1(source_path, kind))
  ocr_pages <- if (is.null(reading_pages)) integer() else reading_pages$page[startsWith(reading_pages$method, "ocr")]
  purrr::map2_chr(conditions$source_page, conditions$quote, function(p, q) {
    if (!nzchar(q)) return("引用なし")
    target <- .normalize_text_c1(q)
    p <- suppressWarnings(as.integer(p))
    if (!is.na(p) && p %in% ocr_pages) return("OCRページ（照合できない）")
    if (!is.na(p) && p >= 1L && p <= length(pages) && grepl(target, pages[p], fixed = TRUE)) return("一致")
    other <- which(purrr::map_lgl(pages, \(t) grepl(target, t, fixed = TRUE)))
    if (length(other)) paste0("別のページにあり（p.", paste(other, collapse = ","), "）") else "PDFに見つからない"
  })
}

# FpML と CSV の一致：主要項目が FpML の該当要素に入っているか、未確認の値が入っていないか
.fpml_targets_c1 <- tibble::tribble(
  ~item_id,                      ~element,
  "trade.trade_date",            "tradeDate",
  "trade.effective_date",        "effectiveDate",
  "trade.termination_date",      "terminationDate",
  "trade.notional_currency",     "currency",
  "funding.floating_rate_index", "floatingRateIndex"
)
.fpml_vs_csv_c1 <- function(fpml_path, conditions) {
  doc <- tryCatch(xml2::read_xml(fpml_path), error = function(e) NULL)
  if (is.null(doc)) return(tibble::tibble())
  purrr::pmap(.fpml_targets_c1, function(item_id, element) {
    row <- dplyr::filter(conditions, .data$item_id == .env$item_id, .data$seq %in% c("1", ""))
    texts <- xml2::xml_text(xml2::xml_find_all(doc, paste0("//*[local-name()='", element, "']")))
    texts <- trimws(texts[nzchar(trimws(texts))])
    stated <- nrow(row) > 0 && row$value_status[1] == "stated" && row$review_status[1] %in% c("confirmed", "corrected")
    value <- if (nrow(row)) row$value[1] else ""
    ok <- if (stated) any(grepl(value, texts, fixed = TRUE)) else !length(texts)
    detail <- if (stated) paste0("CSV「", value, "」／FpML「", paste(texts, collapse = "、"), "」")
              else if (length(texts)) paste0("CSV では未確認なのに FpML に値がある：", paste(texts, collapse = "、"))
              else "CSV で未確認、FpML にも値なし"
    .check_row_c1("FpML", paste0("CSV との一致：", item_id), ok, detail)
  }) |> purrr::list_rbind()
}

validate_c1_case <- function(results, xsd_path = Sys.getenv("CURIE_FPML_XSD", ""), dictionary = trs_field_dictionary_c1()) {
  m <- results$manifest
  modes <- c(copilot = m$copilot_mode %||% NA_character_, claude = m$claude_mode %||% NA_character_)
  run <- .check_row_c1("実行", "PowerShell の処理", identical(as.integer(results$exit_code), 0L),
                       paste0("終了コード ", results$exit_code, "：", c1_exit_code_label(results$exit_code),
                              if (!is.null(m$error)) paste0("（", m$error, "）") else ""))
  if (!identical(as.integer(results$exit_code), 0L) || is.null(results$conditions)) {
    return(list(state = results$ps_state %||% "失敗", checks = run, conditions = results$conditions, modes = modes))
  }
  cond <- results$conditions
  missing_cols <- setdiff(.c1_condition_cols, names(cond))
  if (length(missing_cols)) {
    checks <- dplyr::bind_rows(run, .check_row_c1("CSV", "列", FALSE, paste0("列がない：", paste(missing_cols, collapse = ", "))))
    return(list(state = "要確認", checks = checks, conditions = cond, modes = modes))
  }
  cond <- cond |> dplyr::left_join(dictionary, by = c("item_id" = "field"))
  stated <- cond$value_status == "stated"
  notional_ccy <- dplyr::first(cond$value[cond$item_id == "trade.notional_currency" & stated], default = NA_character_)
  cond <- cond |>
    dplyr::mutate(
      期待する単位 = .expected_unit_c1(.data$item_id, dplyr::coalesce(.data$type, ""), notional_ccy),
      型の確認 = dplyr::if_else(.data$value_status == "stated", .type_ok_c1(.data$value, dplyr::coalesce(.data$type, "text")), NA),
      単位の確認 = dplyr::if_else(.data$value_status == "stated" & nzchar(.data$期待する単位), .data$unit == .data$期待する単位, NA),
      引用の実在確認 = .quote_checks_c1(cond, results$source_path, results$source_kind, results$reading_pages)
    )

  # CSV と JSON の一致
  json_rows <- purrr::map(results$conditions_json$conditions %||% list(), \(r) tibble::as_tibble(purrr::map(r[.c1_condition_cols], \(v) if (is.null(v)) "" else as.character(v)))) |>
    purrr::list_rbind()
  same <- nrow(json_rows) == nrow(results$conditions) &&
    isTRUE(all.equal(as.data.frame(json_rows[.c1_condition_cols]), as.data.frame(results$conditions[.c1_condition_cols]), check.attributes = FALSE))

  key <- paste(cond$item_id, cond$leg_id, cond$seq)
  unknown <- unique(cond$item_id[is.na(cond$section)])
  required <- dictionary$field[dictionary$required]
  absent <- setdiff(required, cond$item_id)
  req_rows <- dplyr::filter(cond, .data$item_id %in% required)
  unresolved_req <- unique(req_rows$item_id[!(req_rows$value_status == "stated" & req_rows$review_status %in% c("confirmed", "corrected"))])
  date_of <- function(id) suppressWarnings(as.Date(dplyr::first(cond$value[cond$item_id == id & stated], default = NA_character_)))
  d <- purrr::map(c(trade = "trade.trade_date", eff = "trade.effective_date", term = "trade.termination_date",
                    final = "return.final_valuation_date"), date_of)
  order_ok <- if (any(is.na(c(d$trade, d$eff, d$term)))) NA else d$trade <= d$eff && d$eff < d$term &&
    (is.na(d$final) || d$final <= d$term)
  product <- dplyr::first(cond$value[cond$item_id == "trade.product_type" & stated], default = NA_character_)

  fpml <- results$files[["fpml"]]
  fpml_check <- if (!is.na(fpml)) validate_fpml_c1(fpml, xsd_path) else NULL
  version_ok <- if (is.null(m$sharepoint_unchanged)) NA else isTRUE(m$sharepoint_unchanged)
  pdf_sha_ok <- if (is.na(results$source_path)) FALSE else identical(pdf_sha256_c1(results$source_path), m$document$sha256)
  ocr_used <- !is.null(results$reading_pages) && any(startsWith(results$reading_pages$method, "ocr"))

  checks <- dplyr::bind_rows(
    run,
    .check_row_c1("CSV", "CSV と JSON の一致", same, if (!same) "行数または値が異なる" else paste0(nrow(cond), " 行")),
    .check_row_c1("条件", "項目IDが定義にある", !length(unknown), unknown),
    .check_row_c1("条件", "項目・レッグ・番号の重複なし", !anyDuplicated(key), unique(key[duplicated(key)])),
    .check_row_c1("条件", "必須項目の行がある", !length(absent), absent),
    .check_row_c1("条件", "原文の状態の値", all(cond$value_status %in% .value_status_levels_c1), unique(setdiff(cond$value_status, .value_status_levels_c1))),
    .check_row_c1("条件", "記載ありの項目に値がある", !any(stated & !nzchar(cond$value)), cond$item_id[stated & !nzchar(cond$value)]),
    .check_row_c1("条件", "記載ありの値の型", all(cond$型の確認 %in% c(TRUE, NA)), cond$item_id[cond$型の確認 %in% FALSE]),
    .check_row_c1("条件", "単位", all(cond$単位の確認 %in% c(TRUE, NA)),
                  paste0(cond$item_id, "（", cond$unit, "、期待 ", cond$期待する単位, "）")[cond$単位の確認 %in% FALSE]),
    .check_row_c1("条件", "日付の順序（約定日≦開始日＜終了日、最終評価日≦終了日）", order_ok,
                  if (is.na(order_ok)) "日付がそろわない" else ""),
    .check_row_c1("条件", "トータル・リターン・スワップである", if (is.na(product)) NA else product == "TOTAL_RETURN_SWAP",
                  dplyr::coalesce(product, "商品の種類が未確認"), na_label = "判定できない"),
    .check_row_c1("確認", "確認状態の値", all(cond$review_status %in% .c1_review_levels), unique(setdiff(cond$review_status, .c1_review_levels))),
    .check_row_c1("確認", "修正した項目に理由がある", !any(cond$review_status == "corrected" & !nzchar(cond$review_note)),
                  cond$item_id[cond$review_status == "corrected" & !nzchar(cond$review_note)]),
    .check_row_c1("確認", "修正（corrected）は Copilot と値が違う項目だけ",
                  !any(cond$review_status == "corrected" & cond$value == cond$copilot_value & nzchar(cond$value)),
                  cond$item_id[cond$review_status == "corrected" & cond$value == cond$copilot_value & nzchar(cond$value)],
                  na_label = "要確認"),
    .check_row_c1("確認", "必須項目が原文で確認された", !length(unresolved_req), unresolved_req),
    .check_row_c1("根拠", "原文引用が指定ページにある", all(cond$引用の実在確認 %in% c("一致", "引用なし", "OCRページ（照合できない）")),
                  paste0(cond$item_id, "：", cond$引用の実在確認)[!cond$引用の実在確認 %in% c("一致", "引用なし", "OCRページ（照合できない）")]),
    .check_row_c1("根拠", "OCR で読んだページの引用（原資料で照合できない）", if (ocr_used) NA else TRUE,
                  if (ocr_used) paste0("OCR のページ：p.", paste(results$reading_pages$page[startsWith(results$reading_pages$method, "ocr")], collapse = ","), "。人が原資料で確認する") else "OCR なし", na_label = "要確認"),
    .check_row_c1("版", "原資料が取得時の記録と一致（SHA-256）", pdf_sha_ok),
    .check_row_c1("版", "読解中に SharePoint 上の資料が変わっていない（eTag）", version_ok,
                  if (is.na(version_ok)) "SharePoint 以外の資料" else "", na_label = "対象外"),
    if (is.null(fpml_check)) .check_row_c1("FpML", "XML構文", FALSE, "fpml.xml がない") else dplyr::bind_rows(
      .check_row_c1("FpML", "XML構文（整形式・ルート要素）", fpml_check$well_formed && fpml_check$root_ok, fpml_check$root_element %|% ""),
      .check_row_c1("FpML", "公式XSD", if (fpml_check$schema_check == "スキーマ適合") TRUE else if (fpml_check$schema_check == "スキーマ不適合") FALSE else NA,
                    paste(fpml_check$schema_check, fpml_check$schema_errors), na_label = "未実施（XSD未設定）"),
      .fpml_vs_csv_c1(fpml, results$conditions)),
    .check_row_c1("出力", "Aladdin 用JSON", NA, "Aladdin の仕様が提示されていないため作らない", na_label = "未作成")
  )
  failed <- any(checks$結果 %in% c("不合格", "要確認"))
  demo <- c(copilot = "Copilot", claude = "Claude")[names(modes)[modes %in% "demo"]]
  state <- if (failed || length(unresolved_req)) "要確認" else if (length(demo)) {
    paste0("デモ（検証は合格。", paste(demo, collapse = "・"), " はモックで、実接続ではない）")
  } else "検証済み"
  list(state = state, checks = checks, conditions = cond, modes = modes, unresolved_required = unresolved_req)
}

`%|%` <- function(a, b) if (is.null(a) || length(a) == 0L || is.na(a)) b else a

# 確認用の Excel（Cambria と MS UI Gothic の文字別フォント。R/z0_utility/excel_format.R）
write_c1_case_excel <- function(results, validation, path = file.path(results$case_dir, "result", "c1_validation.xlsx")) {
  m <- results$manifest
  run <- tibble::tibble(
    項目 = c("案件ID", "取得元", "貼り付けた入力", "入力の種類", "取得日時", "HTTP の状態", "Content-Type", "形式（中身で判定）",
           "原資料の SHA-256", "読解用データ", "読み方", "Copilot のモード", "Copilot への資料の渡し方", "Claude のモード",
           "Copilot API の注意", "状態（R の検証後）", "PowerShell の状態", "終了コード", "SharePoint の eTag（読解前）",
           "会話ID", "Claude の費用（目安 USD）"),
    値 = c(m$case_id %|% "", m$shared_url %|% "", m$pasted_input %|% "", m$source_type %|% "",
          m$acquisition$retrieved_at %|% "", as.character(m$acquisition$http_status %|% ""), m$acquisition$content_type %|% "",
          m$acquisition$format$kind %|% "", m$document$sha256 %|% "", m$stages$reading_data %|% "", m$reading %|% "",
          m$copilot_mode %|% "", m$copilot$document_passed_as %|% "", m$claude_mode %|% "",
          m$copilot_note %|% "", validation$state, results$ps_state %|% "", as.character(results$exit_code),
          m$sharepoint_before$e_tag %|% "", m$copilot$conversation_id %|% "", as.character(m$claude$cost_usd %|% ""))
  )
  conditions <- validation$conditions
  if (!is.null(conditions)) {
    conditions <- conditions |>
      dplyr::transmute(案件ID = .data$case_id, 区分 = .data$section, 項目ID = .data$item_id, 項目 = .data$label_ja,
                       レッグ = .data$leg_id, 番号 = .data$seq, 値 = .data$value, 単位 = .data$unit,
                       原文の状態 = value_status_label_c1(.data$value_status), 根拠ページ = .data$source_page,
                       根拠の位置 = .data$source_location, 原文引用 = .data$quote, 引用の実在確認 = .data$引用の実在確認,
                       Copilotの値 = .data$copilot_value, 確認状態 = .data$review_status, 確認の理由 = .data$review_note)
  }
  write_xlsx_CURIE(purrr::compact(list("実行記録" = run, "検証結果" = validation$checks, "条件" = conditions)), path)
  path
}
