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
  started <- format(Sys.time(), "%Y%m%d_%H%M%S")
  run_dir <- NULL
  if (is.null(notifier)) notifier <- make_demo_notifier_c1(file.path(run_root, "notifications.jsonl"))
  result <- tryCatch({
    # 1. 取得（URLならダウンロード、ローカルなら複製）
    tmp_intake <- file.path(run_root, paste0("_intake_", started))
    acquired <- acquire_trs_document_c1(source, tmp_intake)
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
    agent_dirs <- c(claude = file.path(run_dir, "agents", "claude"), copilot = file.path(run_dir, "agents", "copilot"))
    purrr::iwalk(agent_dirs, \(d, a) prepare_agent_package_c1(a, document, sp_item, d, instruction_path))
    claude_run <- if (run_claude) run_claude_agent_c1(agent_dirs[["claude"]], source) else
      tibble::tibble(agent = "claude", exit_status = NA_integer_, started_at = NA_character_, seconds = NA_real_,
                     cli_version = NA_character_)
    if (!is.null(copilot_dir)) {
      purrr::walk(.agent_files_c1, \(f) if (file.exists(file.path(copilot_dir, f)))
        file.copy(file.path(copilot_dir, f), file.path(agent_dirs[["copilot"]], f), overwrite = TRUE))
    }
    collected <- purrr::imap(agent_dirs, \(d, a) collect_agent_outputs_c1(a, d, pdf_dir, xsd_path))
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
    out_files <- c(list.files(final_dir, recursive = TRUE, full.names = TRUE),
                   list.files(file.path(run_dir, "agents"), recursive = TRUE, full.names = TRUE) |>
                     purrr::discard(\(f) grepl("/pdf/|document[.]pdf$", f)))
    saved <- purrr::map(out_files, function(f) {
      rel <- sub(paste0("^", normalizePath(run_dir, winslash = "/"), "/"), "", normalizePath(f, winslash = "/"))
      client$upload(f, file.path(sharepoint$folder, "output", basename(run_dir), rel))
    }) |> purrr::list_rbind()
    summary <- list(
      status = status, document_id = document_id, source = source, run_dir = normalizePath(run_dir, winslash = "/"),
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
