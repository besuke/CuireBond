# c1：TRSターム条件の抽出結果 → 照合 → 確認表（Excel）・FpML のワークフロー。
#
# 入力：AI（Copilot／Claude）がPDFから作った抽出JSON（docs/c1_trs_extraction_instructions.md の形式）
# 出力：確認表のExcel、文書ごとのFpML（正式版は条件を満たす文書だけ。ドラフト版は不足項目つき）
# PDFからの読取（AIの作業）はこの関数の外で行う。この関数は同じ入力から毎回同じ出力を作る。
# Aladdin用JSONへの変換は、仕様の提示後に追加する。

.c1_table_labels <- c(
  document_id = "文書", section = "区分", field = "項目ID", label_ja = "項目", required = "必須",
  judgement = "判定", judgement_reason = "要確認の理由", status = "照合の状態",
  confirmed_value = "確定値", confirmed_by = "確定の根拠", extracted_value = "抽出値（照合前）",
  page = "ページ", quote = "原文の引用", quote_check = "引用の実在確認", review_note = "確認者のメモ"
)

# 確認表（表示用の列順・日本語見出し）
format_confirmation_table_c1 <- function(table, agents) {
  agent_cols <- unlist(purrr::map(agents, \(a) paste0(c("value_", "value_status_", "confidence_"), a)))
  agent_labels <- unlist(purrr::map(agents, \(a) c(paste0("値（", a, "）"), paste0("原文の状態（", a, "）"),
                                                   paste0("確信度（", a, "）"))))
  table |>
    dplyr::mutate(
      required = dplyr::if_else(.data$required, "必須", "任意"),
      dplyr::across(dplyr::starts_with("value_status_"), value_status_label_c1)
    ) |>
    dplyr::select(dplyr::all_of(c("document_id", "section", "field", "label_ja", "required", "judgement",
                                  "judgement_reason", "status", "confirmed_value", "confirmed_by",
                                  "extracted_value")),
                  dplyr::any_of(agent_cols),
                  dplyr::all_of(c("page", "quote", "quote_check", "review_note"))) |>
    dplyr::rename_with(\(n) dplyr::coalesce(c(.c1_table_labels, rlang::set_names(agent_labels, agent_cols))[n], n))
}

# 文書ごとに正式FpMLを作れるかを判定する
fpml_eligibility_c1 <- function(table, rules_confirmed) {
  purrr::map(split(table, table$document_id), function(d) {
    values <- rlang::set_names(d$confirmed_value, d$field)
    missing <- d$field[d$required & d$judgement != "確定"]
    rules <- rules_confirmed$rule_issue[rules_confirmed$document_id == d$document_id[1]]
    unmapped <- prepare_fpml_values_c1(values[!is.na(values)])$unmapped
    reasons <- c(
      if (length(missing)) paste0("必須項目が未確定 ", length(missing), "件: ", paste(missing, collapse = ", ")),
      if (length(rules)) paste0("規則違反: ", paste(rules, collapse = "／")),
      if (length(unmapped)) paste0("FpMLのコードへ変換できない: ", paste(unmapped, collapse = ", "))
    )
    tibble::tibble(
      document_id = d$document_id[1], file = d$file[1],
      required_total = sum(d$required), required_confirmed = sum(d$required & d$judgement == "確定"),
      fpml_final = length(reasons) == 0L,
      fpml_final_reason = if (length(reasons)) paste(reasons, collapse = "\n") else ""
    )
  }) |> purrr::list_rbind()
}

# extraction_paths：抽出JSON（文書×AIの数だけ）
# pdf_dir       ：元のPDFのフォルダ（引用の実在確認とハッシュ照合に使う。NULLなら確認しない）
# agents        ：照合するAI。1つだけなら「単独抽出」として扱い、照合未実施と明示する
# overrides_path：人による確認結果のCSV（document_id, field, value, reviewer, reviewed_at, reason）
workflows_trs_terms_c1 <- function(extraction_paths, output_dir, pdf_dir = NULL,
                                   agents = c("claude", "copilot"), overrides_path = NULL,
                                   write_draft_fpml = TRUE, output_name = "trs_terms_confirmation") {
  dictionary <- trs_field_dictionary_c1()
  extracted <- read_trs_extractions_c1(extraction_paths, dictionary)
  unknown_agents <- setdiff(unique(extracted$fields$agent), agents)
  if (length(unknown_agents)) stop("agents にないAIの抽出があります: ", paste(unknown_agents, collapse = ", "), call. = FALSE)
  verified <- if (is.null(pdf_dir)) {
    list(fields = dplyr::mutate(extracted$fields, quote_check = "未確認（PDFの指定なし）"),
         hashes = dplyr::mutate(dplyr::distinct(extracted$fields, .data$document_id, .data$file, .data$agent, .data$pdf_sha256),
                                pdf_sha256_actual = NA_character_, hash_check = "未確認（PDFの指定なし）"))
  } else {
    verify_trs_quotes_c1(extracted$fields, pdf_dir)
  }
  overrides <- if (is.null(overrides_path)) NULL else readr::read_csv(overrides_path, col_types = readr::cols(.default = "c"))
  table <- reconcile_trs_terms_c1(verified$fields, agents, dictionary, overrides)
  rules <- dplyr::bind_rows(check_trs_rules_c1(table, "confirmed_value"), check_trs_rules_c1(table, "extracted_value")) |>
    dplyr::distinct(.data$document_id, .data$rule_issue, .keep_all = TRUE)
  eligibility <- fpml_eligibility_c1(table, dplyr::filter(rules, .data$basis == "confirmed_value"))

  # FpML：正式版（条件を満たす文書）とドラフト版（記載のある項目だけ。不足項目つき）
  fpml_dir <- file.path(output_dir, "fpml")
  dir.create(fpml_dir, recursive = TRUE, showWarnings = FALSE)
  fpml_files <- purrr::map(split(table, table$document_id), function(d) {
    id <- d$document_id[1]
    ok <- eligibility$fpml_final[eligibility$document_id == id]
    out <- list()
    if (ok) {
      path <- file.path(fpml_dir, paste0(id, "_FINAL.xml"))
      xml2::write_xml(build_fpml_trs_c1(rlang::set_names(d$confirmed_value, d$field), id, "final"), path)
      out$final <- path
    }
    if (write_draft_fpml && !ok) {
      draft_values <- dplyr::coalesce(d$confirmed_value, d$extracted_value)
      pending <- d$required & d$judgement != "確定"
      path <- file.path(fpml_dir, paste0(id, "_DRAFT.xml"))
      xml2::write_xml(build_fpml_trs_c1(rlang::set_names(draft_values, d$field)[!is.na(draft_values)], id, "draft",
                                         unconfirmed = d$field[pending & !is.na(draft_values)],
                                         not_output = d$field[pending & is.na(draft_values)]), path)
      out$draft <- path
    }
    tibble::tibble(document_id = id, kind = names(out), fpml_file = unlist(out))
  }) |> purrr::list_rbind()
  fpml_files <- fpml_files |>
    dplyr::mutate(sha256 = purrr::map_chr(.data$fpml_file, pdf_sha256_c1),
                  well_formed = purrr::map_lgl(.data$fpml_file, \(f) !inherits(tryCatch(xml2::read_xml(f), error = function(e) e), "error")))

  documents <- eligibility |>
    dplyr::left_join(fpml_files |> dplyr::summarise(fpml = paste(paste0(.data$kind, ": ", basename(.data$fpml_file)), collapse = "\n"),
                                                    .by = "document_id"), by = "document_id") |>
    dplyr::mutate(fpml_status = dplyr::if_else(.data$fpml_final, "正式版を作成", "正式版は未作成（ドラフトのみ）"))

  # 確認表のExcel
  excel_path <- file.path(output_dir, paste0(output_name, ".xlsx"))
  notes <- tibble::tibble(
    項目 = c("目的", "照合", "確定の条件", "原文の状態", "旧参照金利", "FpML", "検証", "Aladdin用JSON"),
    内容 = c(
      "TRSターム条件PDFから抽出した参照インデックス・フィキシング条件などを照合し、確認表とFpMLを作る（価格計算はしない）",
      paste0("照合したAI：", paste(agents, collapse = "・"), if (length(agents) == 1L) "（1つだけのため照合未実施。すべて要確認）" else ""),
      "全AIの値が一致し、引用がPDFの指定ページに実在する項目、または人が確認した項目だけを確定とする",
      "記載あり／空欄／選択肢（未確定）／記載なし を区別する。空欄・選択肢を推測で埋めない",
      "原文に旧参照金利（例：USD-LIBOR-BBA）があっても、現行金利に置き換えず原文どおり記録する",
      "正式版は必須項目がすべて確定し、規則違反がなく、コード変換ができた文書だけ。ドラフト版は正式なFpMLではない",
      "FpMLは整形式（XMLとして読めるか）を確認。公式XSDでの検証は未実施",
      "仕様の提示後に追加する"
    )
  )
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  write_xlsx_CURIE(list(
    "説明" = notes,
    "文書一覧" = documents |> dplyr::transmute(文書 = .data$document_id, ファイル = .data$file,
                                            必須項目 = .data$required_total, うち確定 = .data$required_confirmed,
                                            FpML = .data$fpml_status, 正式版を作れない理由 = .data$fpml_final_reason,
                                            出力ファイル = .data$fpml),
    "確認表" = format_confirmation_table_c1(table, agents),
    "規則チェック" = rules |> dplyr::transmute(文書 = .data$document_id,
                                             対象 = dplyr::recode(.data$basis, confirmed_value = "確定値", extracted_value = "抽出値（照合前）"),
                                             問題 = .data$rule_issue),
    "抽出の実行記録" = extracted$runs |> dplyr::left_join(dplyr::select(verified$hashes, "document_id", "agent", "pdf_sha256_actual", "hash_check"),
                                                     by = c("document_id", "agent")),
    "形式の問題" = extracted$format_issues,
    "FpMLファイル" = fpml_files |> dplyr::mutate(fpml_file = basename(.data$fpml_file)),
    "項目定義" = dictionary
  ), excel_path)
  invisible(list(table = table, documents = documents, rules = rules, fpml_files = fpml_files,
                 runs = extracted$runs, hashes = verified$hashes, format_issues = extracted$format_issues,
                 excel_path = excel_path))
}
