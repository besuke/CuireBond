# c1：AI間の照合、人による確認の反映、規則チェック。
#
# 照合の状態（項目ごと）
#   一致        … 全AIが同じ値を記載ありとして抽出（型に応じて正規化して比較）
#   不一致      … AIの値が違う
#   片方のみ    … 一部のAIだけが値を抽出した
#   単独抽出    … AIが1つだけ（照合していない）
#   空欄／選択肢（未確定）／記載なし … どのAIも値を抽出しなかった（原文の状態で区別）
#   人が確認    … 確認結果（overrides）で値を決めた
# 判定
#   確定        … 「一致」かつ引用がPDFに実在、または「人が確認」
#   要確認      … それ以外（必須項目の空欄・選択肢・記載なし、不一致、単独抽出、引用が見つからない）
#   記載なし（任意項目）… 任意項目でどのAIも値を抽出しなかった
# 推測で値を埋めない。確定値は「確定」の行だけに入れる。

# 型に応じて比較用の値にする（日付はYYYY-MM-DD、数値は数値、コードは大文字）
normalize_term_value_c1 <- function(value, type) {
  if (is.na(value)) return(NA_character_)
  v <- stringr::str_squish(to_halfwidth_CURIE(value))
  out <- switch(type,
    date = tryCatch(format(parse_date_CURIE(v, "date")), error = function(e) NA_character_),
    # 指数表記にしない（1000000000 を 1e+09 にしない）
    number = {
      n <- suppressWarnings(as.numeric(gsub(",", "", v)))
      if (is.na(n)) NA_character_ else format(n, scientific = FALSE, trim = TRUE, digits = 15)
    },
    integer = suppressWarnings(as.character(as.integer(round(as.numeric(gsub(",", "", v)))))),
    code = toupper(v),
    v
  )
  if (is.na(out)) NA_character_ else out
}

# 原文の状態の優先順（AIで状態が分かれたときに表示する代表）
.status_priority_c1 <- c("bracketed", "blank", "not_found")

# fields：verify_trs_quotes_c1()$fields（quote_check 列つき）
# overrides：人による確認結果。列：document_id, field, value, reviewer, reviewed_at, reason
reconcile_trs_terms_c1 <- function(fields, agents = c("claude", "copilot"),
                                   dictionary = trs_field_dictionary_c1(), overrides = NULL) {
  docs <- fields |> dplyr::distinct(.data$document_id, .data$file)
  if (anyDuplicated(docs$document_id)) stop("同じ document_id に異なる file があります。", call. = FALSE)
  if (!"quote_check" %in% names(fields)) fields$quote_check <- NA_character_
  long <- tidyr::expand_grid(docs, field = dictionary$field, agent = agents) |>
    dplyr::left_join(fields |> dplyr::select("document_id", "agent", "field", "value", "value_status", "page",
                                             "quote", "confidence", "quote_check"),
                     by = c("document_id", "agent", "field")) |>
    dplyr::left_join(dictionary, by = "field") |>
    dplyr::mutate(
      value_status = dplyr::coalesce(.data$value_status, "not_found"),
      norm = purrr::map2_chr(.data$value, .data$type, normalize_term_value_c1)
    )
  single <- length(agents) == 1L
  table <- long |>
    dplyr::summarise(
      n_values = sum(!is.na(.data$norm)),
      n_distinct = dplyr::n_distinct(.data$norm[!is.na(.data$norm)]),
      first_value = dplyr::first(stats::na.omit(.data$norm)),
      source_status = {
        s <- unique(.data$value_status)
        dplyr::first(c(.status_priority_c1[.status_priority_c1 %in% s], "not_found"))
      },
      # quote_ok は、下で quote_check を1つの文字列にまとめる前の値で判定する
      quote_ok = all(.data$quote_check[!is.na(.data$norm)] %in% "一致"),
      page = paste(sort(unique(stats::na.omit(.data$page))), collapse = ", "),
      quote = paste(unique(stats::na.omit(.data$quote)), collapse = " ／ "),
      quote_check = paste(unique(stats::na.omit(.data$quote_check[!is.na(.data$quote)])), collapse = " ／ "),
      .by = c("document_id", "file", "field", "section", "label_ja", "type", "required")
    ) |>
    dplyr::mutate(
      # 抽出JSONに行がない項目（指示書の版が古い等）は引用もない
      quote_check = dplyr::if_else(.data$quote_check == "", "引用なし", .data$quote_check),
      status = dplyr::case_when(
        .data$n_values == 0L ~ value_status_label_c1(.data$source_status),
        single ~ "単独抽出",
        .data$n_values < length(agents) ~ "片方のみ",
        .data$n_distinct > 1L ~ "不一致",
        .default = "一致"
      ),
      extracted_value = dplyr::if_else(.data$n_values > 0L & .data$n_distinct == 1L, .data$first_value, NA_character_),
      confirmed_value = dplyr::if_else(.data$status == "一致" & .data$quote_ok, .data$extracted_value, NA_character_),
      confirmed_by = dplyr::if_else(!is.na(.data$confirmed_value), paste(agents, collapse = "・"), NA_character_),
      review_note = NA_character_
    )
  # AIごとの値・状態・確信度を列に並べる（列名は value_claude、value_status_copilot など）
  by_agent <- long |>
    dplyr::select("document_id", "field", "agent", "value", "value_status", "confidence") |>
    tidyr::pivot_wider(names_from = "agent", values_from = c("value", "value_status", "confidence"),
                       names_vary = "slowest")
  table <- table |>
    dplyr::left_join(by_agent, by = c("document_id", "field")) |>
    dplyr::select(-"n_values", -"n_distinct", -"first_value")

  # 人による確認の反映（AIの値は上書きせず、確定値と記録の列にだけ入れる）
  if (!is.null(overrides) && nrow(overrides)) {
    ov <- overrides |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
      dplyr::left_join(dplyr::select(dictionary, "field", "type"), by = "field")
    if (nrow(dplyr::anti_join(ov, table, by = c("document_id", "field")))) {
      stop("確認結果に、確認表にない document_id／field があります。", call. = FALSE)
    }
    ov$norm <- purrr::map2_chr(ov$value, ov$type, normalize_term_value_c1)
    hit <- match(paste(table$document_id, table$field), paste(ov$document_id, ov$field))
    use <- !is.na(hit)
    table$confirmed_value[use] <- ov$norm[hit[use]]
    table$confirmed_by[use] <- paste0("人が確認（", ov$reviewer[hit[use]], "、", ov$reviewed_at[hit[use]], "）")
    table$review_note[use] <- ov$reason[hit[use]]
    table$status[use] <- "人が確認"
  }
  table |>
    dplyr::mutate(
      judgement = dplyr::case_when(
        !is.na(.data$confirmed_value) ~ "確定",
        .data$status %in% c("空欄", "選択肢（未確定）", "記載なし") & !.data$required ~ "記載なし（任意項目）",
        .default = "要確認"
      ),
      judgement_reason = dplyr::case_when(
        .data$judgement != "要確認" ~ "",
        .data$status == "単独抽出" ~ "AIが1つだけで照合していない",
        .data$status == "一致" & !.data$quote_ok ~ paste0("引用がPDFで確認できない（", .data$quote_check, "）"),
        .data$status == "不一致" ~ "AIの値が異なる",
        .data$status == "片方のみ" ~ "一部のAIだけが抽出",
        .data$status == "空欄" ~ "必須項目が空欄（未記入）",
        .data$status == "選択肢（未確定）" ~ "必須項目が選択肢のまま（未確定）",
        .data$status == "記載なし" ~ "必須項目の記載がない",
        .default = ""
      ),
      .after = "status"
    )
}

# 規則チェック（確定値と、照合前の抽出値の両方に対して行う）。問題は document_id ごとの一覧で返す
check_trs_rules_c1 <- function(table, use = c("confirmed_value", "extracted_value")) {
  use <- match.arg(use)
  purrr::map(split(table, table$document_id), function(d) {
    v <- rlang::set_names(d[[use]], d$field)
    date <- function(f) suppressWarnings(as.Date(v[[f]]))
    num <- function(f) suppressWarnings(as.numeric(v[[f]]))
    has <- function(...) all(!is.na(v[c(...)]))
    rules <- c(
      "参照資産と資金調達の参照金利が同じ（取り違えの疑い）" =
        has("funding.floating_rate_index") &&
        toupper(v[["funding.floating_rate_index"]]) %in% toupper(stats::na.omit(c(v[["return.underlyer_name"]], v[["return.underlyer_id"]]))),
      "約定日・効力発生日・終了日の前後関係が不正" =
        has("trade.trade_date", "trade.effective_date", "trade.termination_date") &&
        !(date("trade.trade_date") <= date("trade.effective_date") && date("trade.effective_date") < date("trade.termination_date")),
      "最終評価日が効力発生日〜終了日の範囲外" =
        has("return.final_valuation_date", "trade.effective_date", "trade.termination_date") &&
        !(date("return.final_valuation_date") > date("trade.effective_date") && date("return.final_valuation_date") <= date("trade.termination_date")),
      "フィキシング日のずれが正（リセット日より後）" =
        has("funding.fixing_offset_days") && isTRUE(num("funding.fixing_offset_days") > 0),
      "想定元本が正でない" = has("trade.notional_amount") && isTRUE(num("trade.notional_amount") <= 0),
      "トータル・リターンの受取者と支払者が同じ" =
        has("trade.total_return_receiver", "trade.total_return_payer") &&
        toupper(v[["trade.total_return_receiver"]]) == toupper(v[["trade.total_return_payer"]]),
      "通貨コードが3文字の英字でない" =
        any(purrr::map_lgl(c("trade.notional_currency", "return.underlyer_currency"),
                           \(f) !is.na(v[[f]]) && !grepl("^[A-Z]{3}$", v[[f]])))
    )
    tibble::tibble(document_id = rep(d$document_id[1], sum(rules)), basis = use, rule_issue = names(rules)[rules])
  }) |> purrr::list_rbind()
}
