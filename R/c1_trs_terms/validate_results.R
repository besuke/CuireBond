# c1：PowerShell の成果物（conditions.json・FpML）の検証。
#
# 状態の区別
#   取得失敗／読解失敗／回答形式不正／読解用データ作成失敗 … PowerShell の段階で止まった（終了コードで判断。検証はしない）
#   要確認   … 成果物はあるが、検証の不合格・要確認、必須項目の未確定、または公式XSD の検証が未実施・不適合
#   検証済み … R の検証にすべて合格し、必須項目がすべて原文で確認され、公式XSD にも適合した（Claude が api のとき）
#   デモ     … Claude がデモ（モック）のため、検証に合格しても検証済みとはしない
# FpML は「XML構文」「公式XSD」「条件との一致」を別々に判定する。Aladdin 用JSONは仕様の提示前のため作らない。

.c1_review_levels <- c("confirmed", "unresolved", "not_found")

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

`%|%` <- function(a, b) if (is.null(a) || length(a) == 0L || is.na(a)) b else a

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
    # 文字コード：HTML の宣言（meta charset）→ UTF-8 として正しいか → Windows-1252 の順に決める（PS/common.ps1 と同じ）
    bytes <- readBin(path, "raw", file.size(path))
    head <- rawToChar(bytes[seq_len(min(4096L, length(bytes)))][bytes[seq_len(min(4096L, length(bytes)))] != as.raw(0)])
    declared <- regmatches(head, regexpr("(?i)charset\\s*=\\s*[\"']?\\K[A-Za-z0-9_-]+", head, perl = TRUE))
    text <- rawToChar(bytes[bytes != as.raw(0)])
    encoding <- if (length(declared)) declared else if (validUTF8(text)) "UTF-8" else "windows-1252"
    text <- iconv(text, from = encoding, to = "UTF-8", sub = "?")
    # 古い HTML の &#128;〜&#159;（例：&#147; &#148;）は、ブラウザと同じく Windows-1252 の文字（“ ” など）として読む
    text <- stringr::str_replace_all(text, "&#(1[2-5][0-9]);", \(m) {
      n <- as.integer(sub("&#(\\d+);", "\\1", m))
      ifelse(n >= 128 & n <= 159, iconv(purrr::map_chr(n, \(k) rawToChar(as.raw(k))), "windows-1252", "UTF-8", sub = ""), m)
    })
    doc <- xml2::read_html(text, encoding = "UTF-8")
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
    if (length(other)) paste0("別のページにあり（p.", paste(other, collapse = ","), "）") else "原資料に見つからない"
  })
}

# FpML と条件の一致：主要項目が FpML の該当要素に入っているか、未確認の値が入っていないか
.fpml_targets_c1 <- tibble::tribble(
  ~item_id,                      ~element,
  "trade.trade_date",            "tradeDate",
  "trade.effective_date",        "effectiveDate",
  "trade.termination_date",      "terminationDate",
  "trade.notional_currency",     "currency",
  "funding.floating_rate_index", "floatingRateIndex"
)
.fpml_vs_conditions_c1 <- function(fpml_path, conditions) {
  doc <- tryCatch(xml2::read_xml(fpml_path), error = function(e) NULL)
  if (is.null(doc)) return(tibble::tibble())
  purrr::pmap(.fpml_targets_c1, function(item_id, element) {
    row <- dplyr::filter(conditions, .data$item_id == .env$item_id, .data$seq %in% c("1", ""))
    nodes <- xml2::xml_find_all(doc, paste0("//*[local-name()='", element, "']"))
    # 日付の要素は unadjustedDate の値を見る（営業日調整などの子要素の文字を混ぜない）
    texts <- purrr::map_chr(nodes, function(n) {
      d <- xml2::xml_find_first(n, ".//*[local-name()='unadjustedDate']")
      xml2::xml_text(if (inherits(d, "xml_missing")) n else d)
    })
    texts <- trimws(texts[nzchar(trimws(texts))])
    stated <- nrow(row) > 0 && row$value_status[1] == "stated" && row$review_status[1] == "confirmed"
    value <- if (nrow(row)) row$value[1] else ""
    ok <- if (stated) any(grepl(value, texts, fixed = TRUE)) else !length(texts)
    detail <- if (stated) paste0("条件「", value, "」／FpML「", paste(texts, collapse = "、"), "」")
              else if (length(texts)) paste0("条件では未確認なのに FpML に値がある：", paste(texts, collapse = "、"))
              else "条件で未確認、FpML にも値なし"
    .check_row_c1("FpML", paste0("条件との一致：", item_id), ok, detail)
  }) |> purrr::list_rbind()
}

validate_c1_case <- function(results, xsd_path = Sys.getenv("CURIE_FPML_XSD", ""), dictionary = trs_field_dictionary_c1()) {
  m <- results$manifest
  modes <- c(claude = m$claude_mode %||% NA_character_)
  run <- .check_row_c1("実行", "PowerShell の処理", identical(as.integer(results$exit_code), 0L),
                       paste0("終了コード ", results$exit_code, "：", c1_exit_code_label(results$exit_code),
                              if (!is.null(m$error)) paste0("（", m$error, "）") else ""))
  if (!identical(as.integer(results$exit_code), 0L) || is.null(results$conditions)) {
    return(list(state = results$ps_state %||% "失敗", checks = run, conditions = results$conditions, modes = modes, final = FALSE))
  }
  cond <- results$conditions |> dplyr::left_join(dictionary, by = c("item_id" = "field"))
  stated <- cond$value_status == "stated"
  notional_ccy <- dplyr::first(cond$value[cond$item_id == "trade.notional_currency" & stated], default = NA_character_)
  cond <- cond |>
    dplyr::mutate(
      期待する単位 = .expected_unit_c1(.data$item_id, dplyr::coalesce(.data$type, ""), notional_ccy),
      型の確認 = dplyr::if_else(.data$value_status == "stated", .type_ok_c1(.data$value, dplyr::coalesce(.data$type, "text")), NA),
      単位の確認 = dplyr::if_else(.data$value_status == "stated" & nzchar(.data$期待する単位), .data$unit == .data$期待する単位, NA),
      引用の実在確認 = .quote_checks_c1(cond, results$source_path, results$source_kind, results$reading_pages)
    )

  key <- paste(cond$item_id, cond$leg_id, cond$seq)
  unknown <- unique(cond$item_id[is.na(cond$section)])
  required <- dictionary$field[dictionary$required]
  absent <- setdiff(required, cond$item_id)
  req_rows <- dplyr::filter(cond, .data$item_id %in% required)
  unresolved_req <- unique(req_rows$item_id[!(req_rows$value_status == "stated" & req_rows$review_status == "confirmed")])
  date_of <- function(id) suppressWarnings(as.Date(dplyr::first(cond$value[cond$item_id == id & stated], default = NA_character_)))
  d <- purrr::map(c(trade = "trade.trade_date", eff = "trade.effective_date", term = "trade.termination_date",
                    final = "return.final_valuation_date"), date_of)
  order_ok <- if (any(is.na(c(d$trade, d$eff, d$term)))) NA else d$trade <= d$eff && d$eff < d$term &&
    (is.na(d$final) || d$final <= d$term)
  product <- dplyr::first(cond$value[cond$item_id == "trade.product_type" & stated], default = NA_character_)

  fpml <- results$files[["fpml"]]
  fpml_check <- if (!is.na(fpml)) validate_fpml_c1(fpml, xsd_path) else NULL
  sha_ok <- if (is.na(results$source_path)) FALSE else identical(pdf_sha256_c1(results$source_path), m$document$sha256)
  ocr_used <- !is.null(results$reading_pages) && any(startsWith(results$reading_pages$method, "ocr"))
  ocr_pages <- if (ocr_used) results$reading_pages$page[startsWith(results$reading_pages$method, "ocr")] else integer()
  quote_ok <- c("一致", "引用なし", "OCRページ（照合できない）")

  checks <- dplyr::bind_rows(
    run,
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
    .check_row_c1("確認", "必須項目が原文で確認された", !length(unresolved_req), unresolved_req),
    .check_row_c1("根拠", "原文引用が指定ページにある", all(cond$引用の実在確認 %in% quote_ok),
                  paste0(cond$item_id, "：", cond$引用の実在確認)[!cond$引用の実在確認 %in% quote_ok]),
    .check_row_c1("根拠", "OCR で読んだページの引用（原資料で照合できない）", if (ocr_used) NA else TRUE,
                  if (ocr_used) paste0("OCR のページ：p.", paste(ocr_pages, collapse = ","), "。人が原資料で確認する") else "OCR なし", na_label = "要確認"),
    .check_row_c1("版", "原資料が取得時の記録と一致（SHA-256）", sha_ok),
    if (is.null(fpml_check)) .check_row_c1("FpML", "XML構文", FALSE, "fpml.xml がない") else dplyr::bind_rows(
      .check_row_c1("FpML", "XML構文（整形式・ルート要素）", fpml_check$well_formed && fpml_check$root_ok, fpml_check$root_element %|% ""),
      .check_row_c1("FpML", "公式XSD", if (fpml_check$schema_check == "スキーマ適合") TRUE else if (fpml_check$schema_check == "スキーマ不適合") FALSE else NA,
                    paste(fpml_check$schema_check, fpml_check$schema_errors), na_label = "未実施（XSD未設定）"),
      .fpml_vs_conditions_c1(fpml, results$conditions)),
    .check_row_c1("出力", "Aladdin 用JSON", NA, "Aladdin の仕様が提示されていないため作らない", na_label = "未作成")
  )
  failed <- any(checks$結果 %in% c("不合格", "要確認"))
  # 検証済み（FpML を最終版にする）は、R の検証に合格し、必須項目がそろい、公式XSD にも適合した場合だけ
  xsd_ok <- any(checks$検証 == "公式XSD" & checks$結果 == "合格")
  reasons <- c(if (failed) "検証に不合格・要確認あり", if (length(unresolved_req)) "必須項目が未確定",
               if (!xsd_ok) "FpML の公式XSD 検証が未実施または不適合")
  state <- if (length(reasons)) paste0("要確認（", paste(reasons, collapse = "、"), "）") else if (identical(modes[["claude"]], "demo")) {
    "デモ（検証は合格。Claude はモックで、実際には読んでいない）"
  } else "検証済み"
  list(state = state, checks = checks, conditions = cond, modes = modes, unresolved_required = unresolved_req,
       final = identical(state, "検証済み"))
}
