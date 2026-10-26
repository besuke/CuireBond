# c1：成果物（conditions.json・FpML）の検証。
#
# 4つの判定を別々に出す：XML構文／公式XSD／原文根拠（引用）／条件 JSON との整合（FpML の値）
# 状態
#   取得失敗／読解用データ作成失敗／読解失敗／回答形式不正 … 途中で止まった（検証はしない）
#   要確認   … 成果物はあるが、不合格・要確認・未検証・必須項目の未確定、または公式XSD が未実施・不適合
#   検証済み … 4つの判定がすべて合格し、必須項目がすべて原文で確認された（Claude が api のとき）。FpML は fpml_final.xml
#   デモ     … Claude がデモ（モック）のため、すべて合格しても検証済みとはしない
# Aladdin 送信用 JSON は下書きだけ作る（aladdin_payload.R。送信先・認証が未提示のため送信しない）。

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

# ---- 原文根拠（引用）------------------------------------------------------------------------
# 記載あり・確認済みの値には引用が必要（なければ「引用なし（要確認）」）。記載のない項目は「引用不要」。
# 本文のないページ（スキャン画像など）は、原資料に文字がないため照合できない（要確認）
.quote_ok_c1 <- c("一致", "引用不要")
# 本文のないページの番号
.no_text_pages_c1 <- function(reading_pages) {
  if (is.null(reading_pages)) return(integer())
  reading_pages |> dplyr::filter(.data$method == "no_text") |> dplyr::pull("page")
}
.quote_checks_c1 <- function(conditions, source_path, kind, reading_pages = NULL) {
  if (is.na(source_path)) return(rep("原資料なし", nrow(conditions)))
  pages <- .normalize_text_c1(document_pages_c1(source_path, kind)$text)
  no_text_pages <- .no_text_pages_c1(reading_pages)
  purrr::pmap_chr(list(conditions$value_status, conditions$source_page, conditions$quote), function(status, p, q) {
    if (!nzchar(q)) return(if (identical(status, "stated")) "引用なし（要確認）" else "引用不要")
    target <- .normalize_text_c1(q)
    p <- suppressWarnings(as.integer(p))
    if (!is.na(p) && p %in% no_text_pages) return("本文なしのページ（照合できない）")
    if (!is.na(p) && p >= 1L && p <= length(pages) && grepl(target, pages[p], fixed = TRUE)) return("一致")
    other <- which(purrr::map_lgl(pages, \(t) grepl(target, t, fixed = TRUE)))
    if (length(other)) paste0("別のページにあり（p.", paste(other, collapse = ","), "）") else "原資料に見つからない"
  })
}

# ---- FpML と条件の整合 ----------------------------------------------------------------------
# 項目ごとに、比べる範囲（trade／returnLeg／interestLeg。番号 seq で何番目のレッグかを選ぶ）と要素、比べ方を決める。
# 別のレッグの同じ値や部分一致では合格にしない。対応する要素がなければ「未検証」。
.x_c1 <- function(...) paste0(".//", paste0("*[local-name()='", c(...), "']", collapse = "/"))
.fpml_map_c1 <- tibble::tribble(
  ~item_id,                                  ~scope,         ~xpath,                                                                ~compare,
  "trade.trade_date",                        "trade",        list(.x_c1("tradeDate")),                                              "date",
  "trade.effective_date",                    "return_leg",   list(.x_c1("effectiveDate")),                                          "date",
  "trade.termination_date",                  "return_leg",   list(.x_c1("terminationDate")),                                        "date",
  "trade.notional_amount",                   "return_leg",   list(.x_c1("notionalAmount", "amount")),                               "number",
  "trade.notional_currency",                 "return_leg",   list(.x_c1("notionalAmount", "currency")),                             "upper",
  "return.underlyer_name",                   "return_leg",   list(paste0(.x_c1("underlyer"), "//*[local-name()='description']")),  "text",
  "return.underlyer_id",                     "return_leg",   list(paste0(.x_c1("underlyer"), "//*[local-name()='instrumentId']")), "text",
  "return.valuation_calendar",               "return_leg",   list(.x_c1("valuationDate")),                                          "centers",
  "funding.floating_rate_index",             "interest_leg", list(.x_c1("floatingRateIndex")),                                      "text",
  "funding.index_tenor",                     "interest_leg", list(.x_c1("indexTenor")),                                             "tenor",
  "funding.spread_bp",                       "interest_leg", list(.x_c1("spreadSchedule", "initialValue")),                         "spread_bp",
  "funding.day_count",                       "interest_leg", list(.x_c1("dayCountFraction")),                                       "code:day_count",
  "funding.reset_relative_to",               "interest_leg", list(.x_c1("resetRelativeTo")),                                        "code:reset_relative_to",
  "funding.reset_frequency",                 "interest_leg", list(.x_c1("resetFrequency")),                                         "frequency",
  "funding.fixing_offset_days",              "interest_leg", list(.x_c1("fixingDates", "periodMultiplier")),                        "integer",
  "funding.fixing_calendar",                 "interest_leg", list(.x_c1("fixingDates")),                                            "centers",
  "funding.fixing_business_day_convention",  "interest_leg", list(.x_c1("fixingDates", "businessDayConvention")),                   "code:business_day_convention",
  "funding.payment_frequency",               "interest_leg", list(.x_c1("calculationPeriodFrequency"), .x_c1("paymentFrequency")), "frequency",
  # 支払日は、周期の形（periodicDates の calculationPeriodDatesAdjustments）と相対の形（relativeDates）の両方を見る
  "funding.payment_calendar",                "interest_leg", list(.x_c1("paymentDatesAdjustments"), .x_c1("calculationPeriodDatesAdjustments"),
                                                                 .x_c1("interestLegPaymentDates", "relativeDates")), "centers",
  "funding.payment_business_day_convention", "interest_leg", list(.x_c1("paymentDatesAdjustments", "businessDayConvention"),
                                                                 .x_c1("calculationPeriodDatesAdjustments", "businessDayConvention"),
                                                                 .x_c1("interestLegPaymentDates", "relativeDates", "businessDayConvention")), "code:business_day_convention"
)

# FpML の参照（dateRelativeTo href）：要素が範囲内に直接書かれていないときだけ、明示された参照先（id）の値で比べる。
# 例：interestLegPaymentDates が「returnLegPaymentDates と同じ日」と参照し、営業日調整・カレンダーは参照先にだけ書かれている形。
# たまたま別のレッグに同じ値がある場合とは違い、FpML に書かれた参照だけをたどる（たどった参照は詳細に記録する）
.fpml_refs_c1 <- tibble::tribble(
  ~item_id,                                  ~ref_from,                                                         ~ref_xpath,
  "funding.payment_calendar",                .x_c1("interestLegPaymentDates", "relativeDates", "dateRelativeTo"), ".",
  "funding.payment_business_day_convention", .x_c1("interestLegPaymentDates", "relativeDates", "dateRelativeTo"), ".//*[local-name()='businessDayConvention']"
)
.follow_refs_c1 <- function(doc, leg, item_id, compare) {
  ref <- dplyr::filter(.fpml_refs_c1, .data$item_id == .env$item_id)
  if (!nrow(ref)) return(NULL)
  hrefs <- unique(stats::na.omit(xml2::xml_attr(xml2::xml_find_all(leg, ref$ref_from), "href")))
  if (!length(hrefs)) return(NULL)
  purrr::map(hrefs, function(h) {
    target <- xml2::xml_find_first(doc, paste0("//*[@id='", h, "']"))
    if (inherits(target, "xml_missing")) return(NULL)
    nodes <- if (ref$ref_xpath == ".") list(target) else xml2::xml_find_all(target, ref$ref_xpath)
    v <- purrr::map_chr(nodes, \(nd) .fpml_value_c1(nd, compare))
    list(href = h, found = v[nzchar(v)])
  }) |> purrr::compact() |> purrr::keep(\(r) length(r$found) > 0)
}

# 範囲（レッグ）の要素。seq 番目のレッグ
.fpml_scope_c1 <- function(doc, scope, seq) {
  name <- c(trade = "trade", return_leg = "returnLeg", interest_leg = "interestLeg")[[scope]]
  nodes <- xml2::xml_find_all(doc, paste0("//*[local-name()='", name, "']"))
  if (length(nodes) >= seq) nodes[[seq]] else NULL
}

# FpML の要素から比べる値を取り出す（比べ方に合わせて正規化する）
.fpml_value_c1 <- function(node, compare) {
  child <- function(n) xml2::xml_text(xml2::xml_find_first(node, paste0("./*[local-name()='", n, "']")))
  switch(sub(":.*", "", compare),
         date = { d <- xml2::xml_find_first(node, ".//*[local-name()='unadjustedDate']")
                  stringr::str_squish(xml2::xml_text(if (inherits(d, "xml_missing")) node else d)) },
         tenor = , frequency = toupper(paste0(stringr::str_squish(child("periodMultiplier")), stringr::str_squish(child("period")))),
         centers = paste(sort(unique(stringr::str_squish(xml2::xml_text(xml2::xml_find_all(node, ".//*[local-name()='businessCenter']"))))), collapse = ","),
         stringr::str_squish(xml2::xml_text(node)))
}

# 条件の値を、比べる形にする（変換できなければ NA＝未検証）
.expected_value_c1 <- function(value, compare) {
  maps <- .fpml_code_maps_c1
  switch(sub(":.*", "", compare),
         date = value, upper = toupper(stringr::str_squish(value)), text = stringr::str_squish(value),
         number = value, integer = value, spread_bp = value,
         tenor = toupper(stringr::str_squish(value)),
         frequency = .map_code_c1(maps$frequency, value),
         code = .map_code_c1(maps[[sub("^code:", "", compare)]], value),
         centers = { c <- .business_centers_c1(value); if (!length(c) || anyNA(c)) NA_character_ else paste(sort(unique(c)), collapse = ",") })
}

.same_value_c1 <- function(expected, found, compare) {
  switch(sub(":.*", "", compare),
         number = isTRUE(abs(as.numeric(found) - as.numeric(expected)) <= 1e-9 * max(1, abs(as.numeric(expected)))),
         integer = identical(suppressWarnings(as.integer(found)), suppressWarnings(as.integer(expected))),
         spread_bp = isTRUE(abs(as.numeric(found) * 10000 - as.numeric(expected)) <= 1e-6),
         text = identical(toupper(found), toupper(expected)),
         identical(found, expected))
}

.fpml_vs_conditions_c1 <- function(fpml_path, conditions) {
  doc <- tryCatch(xml2::read_xml(fpml_path), error = function(e) NULL)
  if (is.null(doc)) return(tibble::tibble())
  purrr::pmap(.fpml_map_c1, function(item_id, scope, xpath, compare) {
    rows <- dplyr::filter(conditions, .data$item_id == .env$item_id)
    if (!nrow(rows)) rows <- tibble::tibble(item_id = item_id, seq = "1", value = "", value_status = "not_found", review_status = "not_found")
    purrr::pmap(list(rows$seq, rows$value, rows$value_status, rows$review_status), function(seq, value, status, review) {
      n <- suppressWarnings(as.integer(seq)) %|% 1L
      label <- paste0("条件との整合：", item_id, if (n > 1L) paste0("（", n, "番目）") else "")
      leg <- .fpml_scope_c1(doc, scope, n)
      nodes <- if (is.null(leg)) list() else purrr::detect(purrr::map(xpath, \(x) xml2::xml_find_all(leg, x)), \(s) length(s) > 0) %||% list()
      found <- purrr::map_chr(nodes, \(nd) .fpml_value_c1(nd, compare))
      found <- found[nzchar(found)]
      via <- character()
      if (!length(found) && !is.null(leg)) {
        refs <- .follow_refs_c1(doc, leg, item_id, compare)
        if (length(refs)) {
          found <- unlist(purrr::map(refs, "found"))
          via <- purrr::map_chr(refs, "href")
        }
      }
      confirmed <- identical(status, "stated") && identical(review, "confirmed")
      if (!confirmed) {
        ok <- !length(found)
        return(.check_row_c1("FpML", label, if (ok) TRUE else FALSE,
                             if (ok) "条件で未確認、FpML にも値なし" else paste0("条件では未確認なのに FpML に値がある：", paste(found, collapse = "、"))))
      }
      expected <- .expected_value_c1(value, compare)
      if (is.na(expected)) return(.check_row_c1("FpML", label, NA, paste0("条件「", value, "」を FpML のコードに変換できない"), na_label = "未検証"))
      if (is.null(leg)) return(.check_row_c1("FpML", label, NA, paste0("FpML に ", scope, " がない"), na_label = "未検証"))
      if (!length(found)) return(.check_row_c1("FpML", label, NA, "FpML に対応する要素がない", na_label = "未検証"))
      ok <- all(purrr::map_lgl(found, \(f) .same_value_c1(expected, f, compare)))
      .check_row_c1("FpML", label, ok, paste0("条件「", value, "」→「", expected, "」／FpML「", paste(found, collapse = "、"), "」",
                                              if (length(via)) paste0("（参照 dateRelativeTo → ", paste(via, collapse = "、"), " をたどった）") else ""))
    }) |> purrr::list_rbind()
  }) |> purrr::list_rbind()
}

# ---- 検証の本体 -----------------------------------------------------------------------------
validate_c1_case <- function(results, xsd_path = Sys.getenv("CURIE_FPML_XSD", ""), dictionary = trs_field_dictionary_c1()) {
  m <- results$manifest
  modes <- c(claude = m$claude_mode %||% NA_character_)
  f <- results$failure
  run <- .check_row_c1("実行", "取得・読解", is.null(f), if (is.null(f)) "完了" else paste0(f$kind, "：", f$message))
  if (!is.null(f) || is.null(results$conditions)) {
    return(list(state = if (!is.null(f)) f$kind else "失敗", checks = run, conditions = results$conditions, modes = modes,
                verdicts = list(), final = FALSE))
  }
  cond <- results$conditions |> dplyr::left_join(dictionary, by = c("item_id" = "field"))
  # 記載ありの値（なければ NA）
  stated_value <- function(id) {
    cond |> dplyr::filter(.data$item_id == id, .data$value_status == "stated") |> dplyr::pull("value") |> dplyr::first(default = NA_character_)
  }
  notional_ccy <- stated_value("trade.notional_currency")
  cond <- cond |>
    dplyr::mutate(
      期待する単位 = .expected_unit_c1(.data$item_id, dplyr::coalesce(.data$type, ""), notional_ccy),
      型の確認 = dplyr::if_else(.data$value_status == "stated", .type_ok_c1(.data$value, dplyr::coalesce(.data$type, "text")), NA),
      単位の確認 = dplyr::if_else(.data$value_status == "stated" & nzchar(.data$期待する単位), .data$unit == .data$期待する単位, NA),
      引用の実在確認 = .quote_checks_c1(cond, results$source_path, results$source_kind, results$reading_pages)
    )

  # 条件に当てはまる項目ID（重複なし）、条件に当てはまる行の説明
  ids_where <- function(...) cond |> dplyr::filter(...) |> dplyr::distinct(.data$item_id) |> dplyr::pull("item_id")
  labels_where <- function(label, ...) cond |> dplyr::filter(...) |> dplyr::transmute(x = {{ label }}) |> dplyr::pull("x")

  duplicated_keys <- cond |> dplyr::count(.data$item_id, .data$leg_id, .data$seq) |> dplyr::filter(.data$n > 1L) |>
    dplyr::transmute(key = paste(.data$item_id, .data$leg_id, .data$seq)) |> dplyr::pull("key")
  unknown <- ids_where(is.na(.data$section))
  stated_empty <- ids_where(.data$value_status == "stated", !nzchar(.data$value))
  type_bad <- ids_where(.data$型の確認 %in% FALSE)
  unit_bad <- labels_where(paste0(.data$item_id, "（", .data$unit, "、期待 ", .data$期待する単位, "）"), .data$単位の確認 %in% FALSE)
  required <- dictionary |> dplyr::filter(.data$required) |> dplyr::pull("field")
  absent <- setdiff(required, cond$item_id)
  unresolved_req <- ids_where(.data$item_id %in% required, !(.data$value_status == "stated" & .data$review_status == "confirmed"))
  d <- purrr::map(c(trade = "trade.trade_date", eff = "trade.effective_date", term = "trade.termination_date",
                    final = "return.final_valuation_date"), \(id) suppressWarnings(as.Date(stated_value(id))))
  order_ok <- if (any(is.na(c(d$trade, d$eff, d$term)))) NA else d$trade <= d$eff && d$eff < d$term &&
    (is.na(d$final) || d$final <= d$term)
  product <- stated_value("trade.product_type")
  sha_ok <- if (is.na(results$source_path)) FALSE else identical(pdf_sha256_c1(results$source_path), m$document$sha256)
  no_text_pages <- .no_text_pages_c1(results$reading_pages)

  # 原文根拠：引用の確認結果の分類
  quote_missing <- ids_where(.data$引用の実在確認 == "引用なし（要確認）")
  quote_wrong <- labels_where(paste0(.data$item_id, "：", .data$引用の実在確認),
                              !.data$引用の実在確認 %in% c(.quote_ok_c1, "引用なし（要確認）", "本文なしのページ（照合できない）"))
  # FpML
  fpml <- results$files[["fpml"]]
  fpml_check <- if (!is.na(fpml)) validate_fpml_c1(fpml, xsd_path) else NULL
  fpml_rows <- if (!is.null(fpml_check) && fpml_check$well_formed) .fpml_vs_conditions_c1(fpml, results$conditions) else tibble::tibble()

  checks <- dplyr::bind_rows(
    run,
    .check_row_c1("条件", "項目IDが定義にある", !length(unknown), unknown),
    .check_row_c1("条件", "項目・レッグ・番号の重複なし", !length(duplicated_keys), duplicated_keys),
    .check_row_c1("条件", "必須項目の行がある", !length(absent), absent),
    .check_row_c1("条件", "原文の状態の値", all(cond$value_status %in% .value_status_levels_c1), unique(setdiff(cond$value_status, .value_status_levels_c1))),
    .check_row_c1("条件", "記載ありの項目に値がある", !length(stated_empty), stated_empty),
    .check_row_c1("条件", "記載ありの値の型", !length(type_bad), type_bad),
    .check_row_c1("条件", "単位", !length(unit_bad), unit_bad),
    .check_row_c1("条件", "日付の順序（約定日≦開始日＜終了日、最終評価日≦終了日）", order_ok,
                  if (is.na(order_ok)) "日付がそろわない" else ""),
    .check_row_c1("条件", "トータル・リターン・スワップである", if (is.na(product)) NA else product == "TOTAL_RETURN_SWAP",
                  dplyr::coalesce(product, "商品の種類が未確認"), na_label = "判定できない"),
    .check_row_c1("確認", "確認状態の値", all(cond$review_status %in% .c1_review_levels), unique(setdiff(cond$review_status, .c1_review_levels))),
    .check_row_c1("確認", "必須項目が原文で確認された", !length(unresolved_req), unresolved_req),
    .check_row_c1("根拠", "記載ありの値に原文の引用がある", if (length(quote_missing)) NA else TRUE,
                  if (length(quote_missing)) paste0("引用なし：", paste(quote_missing, collapse = ", ")) else "", na_label = "要確認"),
    .check_row_c1("根拠", "原文引用が指定ページにある", !length(quote_wrong), quote_wrong),
    .check_row_c1("根拠", "本文のないページの引用（原資料で照合できない）", if (length(no_text_pages)) NA else TRUE,
                  if (length(no_text_pages)) paste0("本文のないページ：p.", paste(no_text_pages, collapse = ","), "。人が原資料で確認する") else "本文のないページなし", na_label = "要確認"),
    .check_row_c1("版", "原資料が取得時の記録と一致（SHA-256）", sha_ok),
    if (is.null(fpml_check)) .check_row_c1("FpML", "XML構文", FALSE, "fpml.xml がない") else dplyr::bind_rows(
      .check_row_c1("FpML", "XML構文（整形式・ルート要素）", fpml_check$well_formed && fpml_check$root_ok, fpml_check$root_element %|% ""),
      .check_row_c1("FpML", "公式XSD", if (fpml_check$schema_check == "スキーマ適合") TRUE else if (fpml_check$schema_check == "スキーマ不適合") FALSE else NA,
                    paste(fpml_check$schema_check, fpml_check$schema_errors), na_label = "未実施（XSD未設定）"),
      fpml_rows),
    .check_row_c1("出力", "Aladdin への送信", NA, "送信用 JSON は下書きだけ作る（aladdin_request_DRAFT.json）。送信先・認証が未提示のため送信しない", na_label = "未送信")
  )

  # 4つの判定（別々に出す）
  verdict_of <- function(rows) {
    if (!nrow(rows)) return("未実施")
    pending <- rows |> dplyr::filter(.data$結果 %in% c("要確認", "未検証", "未実施（XSD未設定）")) |> dplyr::pull("結果")
    if (any(rows$結果 == "不合格")) "不合格" else if (length(pending)) pending[[1]] else "合格"
  }
  verdicts <- list(
    XML構文 = verdict_of(dplyr::filter(checks, .data$検証 == "XML構文（整形式・ルート要素）" | .data$検証 == "XML構文")),
    公式XSD = verdict_of(dplyr::filter(checks, .data$検証 == "公式XSD")),
    原文根拠 = verdict_of(dplyr::filter(checks, .data$区分 == "根拠")),
    条件JSONとの整合 = verdict_of(dplyr::filter(checks, startsWith(.data$検証, "条件との整合")))
  )
  other_bad <- any(dplyr::filter(checks, !.data$区分 %in% c("根拠", "FpML", "出力"))$結果 %in% c("不合格", "要確認"))
  reasons <- c(purrr::imap_chr(purrr::keep(verdicts, \(v) v != "合格"), \(v, k) paste0(k, "：", v)),
               if (other_bad) "条件の検証に不合格・要確認あり", if (length(unresolved_req)) "必須項目が未確定")
  state <- if (length(reasons)) paste0("要確認（", paste(reasons, collapse = "、"), "）") else if (identical(modes[["claude"]], "demo")) {
    "デモ（検証は合格。Claude はモックで、実際には読んでいない）"
  } else "検証済み"
  list(state = state, checks = checks, conditions = cond, modes = modes, verdicts = verdicts,
       unresolved_required = unresolved_req, final = identical(state, "検証済み"))
}
