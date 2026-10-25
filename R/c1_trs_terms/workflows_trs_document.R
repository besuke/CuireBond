# c1：TRSターム条件PDF 1件の処理（取得 → SharePoint配置 → 独立読解 → 照合 → 最終成果物 → 保存 → 通知）。
#
# source         ：PDFのURL、またはローカルファイルのパス
# run_root       ：実行ごとのフォルダを作る場所（既定 output/c1_trs/runs）
# sharepoint     ：sharepoint_settings_c1()。既定はデモ（ローカルフォルダ）
# upload_input   ："never"（既定）：入力PDFは SharePoint に配置しない（AIには元のURL・ファイルを渡す）
#                  "auto"：ローカルファイルだけ配置する。"always"：URLでも配置する（成果物はどれでも保存する）
# run_claude     ：TRUE なら Claude Code を非対話モードで実行して読ませる
# copilot_dir    ：Copilot の出力（extraction.json 等）を置いたフォルダ。NULL なら作業フォルダの agents/copilot を見る
# overrides_path ：人による確認結果のCSV（確認待ちの解消に使う）
# notifier       ：function(event)。既定はデモの通知（コンソールと通知ログ）
#
# フォルダ構成（1回の実行）
#   intake/             取得したPDFと取得記録
#   agents/claude/      Claude の作業フォルダと出力（AIの出力として保存）
#   agents/copilot/     Copilot への依頼文と出力（AIの出力として保存）
#   final/              照合後の最終成果物（確認表Excel、FpML、検証結果）
#   run_summary.json    実行の記録（状態、保存先、理由）

# コンソールに表示する主要項目（日付・参照レグ・参照金利・カレンダー。金額の項目は含めない）
.key_fields_c1 <- c(
  "trade.product_type",
  "trade.trade_date", "trade.effective_date", "trade.termination_date", "return.final_valuation_date",
  "return.underlyer_type", "return.underlyer_name", "return.underlyer_id", "return.underlyer_issuer", "return.return_type",
  "funding.floating_rate_index", "funding.index_tenor", "funding.reset_relative_to", "funding.fixing_offset_days",
  "return.valuation_calendar", "funding.fixing_calendar", "funding.payment_calendar",
  "funding.fixing_business_day_convention", "funding.payment_business_day_convention"
)

# 項目ごとに「取得できたもの」「取得できなかったもの」をまとめる（コンソール表示・確認用）
# fields：表示する項目（既定は主要項目。NULLなら全項目）
summarize_terms_c1 <- function(result, fields = .key_fields_c1) {
  dict <- trs_field_dictionary_c1()
  order_of <- function(x) if (is.null(fields)) x else x |>
    dplyr::filter(.data$field %in% fields) |> dplyr::arrange(match(.data$field, fields))
  dict <- order_of(dict)
  table <- result$final$table
  if (is.null(table)) {
    # AIの抽出結果が1つもない：すべて取得できなかった（AIの状態を理由にする）
    states <- if (is.null(result$collected)) "処理が失敗した" else
      paste(names(result$collected), purrr::map_chr(result$collected, "state"), sep = "：", collapse = "／")
    return(dict |> dplyr::transmute(区分 = .data$section, 項目 = .data$label_ja,
                                    必須 = dplyr::if_else(.data$required, "必須", "任意"),
                                    結果 = "取得できなかった", 値 = NA_character_, ページ = NA_character_, 理由 = states))
  }
  order_of(table) |>
    dplyr::mutate(
      value = dplyr::coalesce(.data$confirmed_value, .data$extracted_value),
      結果 = dplyr::if_else(is.na(.data$value), "取得できなかった", "取得できた"),
      理由 = dplyr::case_when(
        !is.na(.data$confirmed_value) ~ paste0("確定（", .data$confirmed_by, "）"),
        !is.na(.data$value) ~ paste0("要確認：", .data$judgement_reason),
        .data$status == "空欄" ~ "原文が空欄（未記入）",
        .data$status == "選択肢（未確定）" ~ "原文が選択肢のまま（未確定）",
        .data$status == "記載なし" ~ "原文に記載なし",
        .data$status == "不一致" ~ "AIの値が異なる",
        .default = .data$judgement_reason
      )
    ) |>
    dplyr::transmute(区分 = .data$section, 項目 = .data$label_ja, 必須 = dplyr::if_else(.data$required, "必須", "任意"),
                     結果 = .data$結果, 値 = .data$value, ページ = dplyr::na_if(.data$page, ""), 理由 = .data$理由)
}

# そもそもTRSか：AIが読んだ商品の種類（trade.product_type）と根拠を返す。推測では判定しない
# answer：「はい」「いいえ」「判定できない」
trs_product_check_c1 <- function(result) {
  row <- result$final$table
  if (!is.null(row)) row <- dplyr::filter(row, .data$field == "trade.product_type")
  if (is.null(row) || !nrow(row)) {
    return(list(answer = "判定できない", value = NA_character_, page = NA_character_, quote = NA_character_,
                reason = "AIの抽出結果に商品の種類がない"))
  }
  value <- dplyr::coalesce(row$confirmed_value, row$extracted_value)[1]
  answer <- dplyr::case_when(is.na(value) ~ "判定できない",
                             toupper(value) == "TOTAL_RETURN_SWAP" ~ "はい",
                             .default = "いいえ")
  list(answer = answer, value = value, page = dplyr::na_if(row$page[1], ""), quote = dplyr::na_if(row$quote[1], ""),
       reason = if (!is.na(row$confirmed_value[1])) "確定" else paste0("要確認：", row$judgement_reason[1]))
}

.document_id_c1 <- function(file_name) {
  id <- tools::file_path_sans_ext(file_name)
  gsub("[^A-Za-z0-9_-]", "_", id)
}

workflows_trs_document_c1 <- function(
    source, run_root = file.path("output", "c1_trs", "runs"),
    sharepoint = sharepoint_settings_c1(), upload_input = c("never", "auto", "always"),
    run_claude = TRUE, copilot_dir = NULL,
    overrides_path = NULL, notifier = NULL, xsd_path = Sys.getenv("CURIE_FPML_XSD", ""),
    instruction_path = file.path("docs", "c1_trs_extraction_instructions.md")) {
  upload_input <- match.arg(upload_input)
  pasted_source <- source
  source <- strip_tracking_params_c1(source)   # utm_ で始まる追跡用パラメータを除く（元のURLは記録に残す）
  started <- format(Sys.time(), "%Y%m%d_%H%M%S")
  run_dir <- NULL
  # いま何をしているかをコンソールに表示する
  step <- function(...) message(format(Sys.time(), "%H:%M:%S"), " [c1] ", ...)
  if (is.null(notifier)) notifier <- make_demo_notifier_c1(file.path(run_root, "notifications.jsonl"))
  result <- tryCatch({
    # 1. 取得（URLならダウンロード、ローカルなら複製）
    step("1/5 PDFを取得しています：", source)
    tmp_intake <- file.path(run_root, paste0("_intake_", started))
    acquired <- acquire_trs_document_c1(source, tmp_intake)
    step("    取得しました：", acquired$file_name, "（", acquired$pages, "ページ、SHA-256 ", substr(acquired$sha256, 1, 12), "…）")
    document_id <- .document_id_c1(acquired$file_name)
    run_dir <- file.path(run_root, paste0(document_id, "_", started))
    dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
    file.rename(tmp_intake, file.path(run_dir, "intake"))
    acquired$stored_path <- normalizePath(file.path(run_dir, "intake", basename(acquired$stored_path)), winslash = "/")
    document <- c(as.list(acquired), document_id = document_id)

    # 2. 入力PDFの共有先を決める
    #    ローカルファイル：SharePoint へ配置し、同じ版を取り直してハッシュを確かめる
    #    URL：元のURLを共有先とする（SharePoint へは上げない。upload_input = "always" なら上げる）
    client <- make_sharepoint_client_c1(sharepoint)
    upload <- switch(upload_input, always = TRUE, never = FALSE, auto = !.is_url_c1(source))
    if (upload) {
      remote_pdf <- file.path(sharepoint$folder, "input", paste0(substr(acquired$sha256, 1, 12), "_", acquired$file_name))
      sp_item <- client$upload(acquired$stored_path, remote_pdf)
      fetched <- client$download(remote_pdf, tempfile(fileext = ".pdf"))
      if (pdf_sha256_c1(fetched) != acquired$sha256) stop("SharePoint から取得したPDFが配置したものと一致しません。", call. = FALSE)
    } else {
      sp_item <- tibble::tibble(remote_path = NA_character_, url = source, item_id = NA_character_,
                                sha256 = acquired$sha256, uploaded_at = NA_character_,
                                connection = paste0("元の場所（SharePointには配置しない。R の取得日時 ", acquired$retrieved_at, "）"))
      fetched <- acquired$stored_path
    }
    pdf_dir <- file.path(run_dir, "agents", "pdf")
    dir.create(pdf_dir, recursive = TRUE, showWarnings = FALSE)
    file.copy(fetched, file.path(pdf_dir, acquired$file_name), overwrite = TRUE)
    document$stored_path <- file.path(pdf_dir, acquired$file_name)

    # 3. 独立読解（AIごとのフォルダ）
    step("2/5 ClaudeとCopilotへの依頼文を作っています")
    agent_dirs <- c(claude = file.path(run_dir, "agents", "claude"), copilot = file.path(run_dir, "agents", "copilot"))
    purrr::iwalk(agent_dirs, \(d, a) prepare_agent_package_c1(a, document, sp_item, d, instruction_path))
    if (run_claude) step("3/5 Claude Code にPDFを読ませています（数分かかります。この間は表示が止まります）")
    claude_run <- if (run_claude) run_claude_agent_c1(agent_dirs[["claude"]], source) else
      tibble::tibble(agent = "claude", exit_status = NA_integer_, started_at = NA_character_, seconds = NA_real_,
                     cli_version = NA_character_)
    if (!is.null(copilot_dir)) {
      purrr::walk(.agent_files_c1, \(f) if (file.exists(file.path(copilot_dir, f)))
        file.copy(file.path(copilot_dir, f), file.path(agent_dirs[["copilot"]], f), overwrite = TRUE))
    }
    if (run_claude) step("    Claude が終わりました（", claude_run$seconds, "秒、費用 約", round(claude_run$cost_usd, 2), " USD）")
    step("4/5 AIの出力を取り込み、照合しています")
    collected <- purrr::imap(agent_dirs, \(d, a) collect_agent_outputs_c1(a, d, pdf_dir, xsd_path))
    purrr::iwalk(collected, \(x, a) step("    ", a, "：", x$state))
    available <- purrr::compact(purrr::map(collected, "extraction"))

    # 4. 照合 → 最終成果物（両AIの出力がそろわなければ、照合は「片方のみ」となり確認待ち）
    final_dir <- file.path(run_dir, "final")
    reasons <- character()
    final <- NULL
    if (length(available)) {
      final <- workflows_trs_terms_c1(unname(unlist(available)), final_dir, pdf_dir = pdf_dir,
                                      agents = names(agent_dirs), overrides_path = overrides_path,
                                      output_name = paste0(document_id, "_confirmation"))
      final$schema <- purrr::map(final$fpml_files$fpml_file, \(p) validate_fpml_c1(p, xsd_path)) |> purrr::list_rbind()
      write_xlsx_CURIE(list("FpML検証" = final$schema), file.path(final_dir, "fpml_validation.xlsx"))
      if (!all(final$documents$fpml_final)) reasons <- c(reasons, final$documents$fpml_final_reason)
    } else {
      reasons <- c(reasons, "AIの抽出結果が1つもない")
    }
    missing_agents <- names(collected)[purrr::map_lgl(collected, \(x) is.null(x$extraction))]
    if (length(missing_agents)) {
      reasons <- c(reasons, paste0("未取得のAI出力: ", paste(purrr::map_chr(collected[missing_agents], "state"), collapse = "／")))
    }
    status <- if (!is.null(final) && all(final$documents$fpml_final) && !length(missing_agents)) "完了" else "要確認"

    # 5. 成果物を SharePoint に保存し、通知する
    step("5/5 成果物を保存し、通知しています（判定：", status, "）")
    out_files <- c(list.files(final_dir, recursive = TRUE, full.names = TRUE),
                   list.files(file.path(run_dir, "agents"), recursive = TRUE, full.names = TRUE) |>
                     purrr::discard(\(f) grepl("/pdf/|document[.]pdf$", f)))
    saved <- purrr::map(out_files, function(f) {
      rel <- sub(paste0("^", normalizePath(run_dir, winslash = "/"), "/"), "", normalizePath(f, winslash = "/"))
      client$upload(f, file.path(sharepoint$folder, "output", basename(run_dir), rel))
    }) |> purrr::list_rbind()
    summary <- list(
      status = status, document_id = document_id, source = source, pasted_source = pasted_source,
      run_dir = normalizePath(run_dir, winslash = "/"),
      input = list(file = acquired$file_name, sha256 = acquired$sha256, retrieved_at = acquired$retrieved_at,
                   method = acquired$method, sharepoint_url = sp_item$url, sharepoint_item_id = sp_item$item_id,
                   sharepoint_connection = sp_item$connection),
      agents = purrr::imap(collected, \(x, a) list(state = x$state)),
      claude_run = as.list(claude_run),
      outputs = saved$url, reasons = reasons
    )
    jsonlite::write_json(summary, file.path(run_dir, "run_summary.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")
    notifier(list(status = status, document_id = document_id, source = source,
                  locations = c(sp_item$url, saved$url[grepl("/final/", saved$url)]), reasons = reasons,
                  time = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")))
    list(status = status, summary = summary, final = final, collected = collected, acquired = acquired,
         sharepoint_item = sp_item, saved = saved, run_dir = run_dir)
  }, error = function(e) {
    notifier(list(status = "失敗", document_id = if (!is.null(run_dir)) basename(run_dir) else NA_character_,
                  source = source, locations = if (!is.null(run_dir)) normalizePath(run_dir, winslash = "/", mustWork = FALSE) else character(),
                  reasons = conditionMessage(e), time = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")))
    list(status = "失敗", error = conditionMessage(e), run_dir = run_dir)
  })
  invisible(result)
}
