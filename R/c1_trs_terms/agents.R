# c1：Claude と Microsoft 365 Copilot による独立読解。
#
# 両者に同じ対象（URL、またはローカルファイル）と同じ指示書を渡し、AI自身に読ませる。
# R が本文を抜き出してAIに渡すことはしない。AIが読めなかった場合は、読めなかったと記録して終える。
# それぞれ次を作らせる（AIごとのフォルダに保存し、照合後の最終成果物とは分ける）。
#   extraction.json   … 条件一覧（共通JSON。docs/c1_trs_extraction_instructions.md の形式）
#   fpml_proposal.xml … FpMLへの変換案（AIが作ったもの。Rで構文・スキーマを検証する）
#   review_items.md   … 要確認事項と根拠ページ（読めなかった場合はその理由）
# Excelの条件一覧は、AIの extraction.json から R が作る（書式・列構成をそろえるため）。
# R は照合のためだけに同じ対象を取得し、SHA-256 を記録して引用の実在を確かめる。
#
# Claude：Claude Code を非対話モード（claude -p）で起動する。URLなら、そのドメインだけウェブ取得を許可する。
#         ローカルファイルなら作業フォルダにPDFを置く。読み書きは作業フォルダ内だけ。
# Copilot：R から直接実行する手段が未設定。依頼文（同じURLと指示）を作り、
#          Copilot の出力を所定のフォルダに置いてもらって取り込む（未取得なら確認待ち）。

.agent_files_c1 <- c(extraction = "extraction.json", fpml = "fpml_proposal.xml", review = "review_items.md")

# AIごとの作業フォルダを作る（指示書・PDF・依頼文）
prepare_agent_package_c1 <- function(agent, document, sharepoint_item, agent_dir,
                                     instruction_path = file.path("docs", "c1_trs_extraction_instructions.md")) {
  dir.create(agent_dir, recursive = TRUE, showWarnings = FALSE)
  file.copy(instruction_path, file.path(agent_dir, "instructions.md"), overwrite = TRUE)
  header <- c(
    "# CURIE c1 TRSターム条件の読解依頼",
    "",
    "あなたは CURIE c1 のTRSターム条件の読解担当です。他のAIの結果は見ずに、独立して読んでください。",
    "instructions.md（抽出指示）に従い、対象PDFだけを根拠にしてください。推測で補わないでください。",
    "",
    "## 対象PDF",
    paste0("- 共有先: ", sharepoint_item$url, "（", sharepoint_item$connection, "）"),
    paste0("- R の取得日時: ", document$retrieved_at, "（取得元 ", document$source, "）"),
    paste0("- ファイル名: ", document$file_name, "（", document$pages, "ページ）"),
    paste0("- SHA-256: ", document$sha256),
    "",
    "## 作るファイル（このフォルダに保存）",
    paste0("1. `extraction.json`：instructions.md の形式。document は document_id=", document$document_id,
           "、file=", document$file_name, "、pages=", document$pages, "、pdf_sha256=", document$sha256,
           "、extracted_by=", agent, "、instruction_version=c1-extract-v1。model にモデル名、extracted_at に現在日時"),
    "2. `fpml_proposal.xml`：extraction.json の「記載あり」の値だけで作る FpML 5.x confirmation の returnSwap の変換案。",
    "   空欄・選択肢・記載なしの項目は要素を作らず、XMLコメントに列挙する。推測で埋めない",
    "3. `review_items.md`：要確認事項（空欄・選択肢・記載なし・解釈に迷った点）と根拠ページの一覧",
    ""
  )
  unreadable <- c(
    "## 読めなかった場合",
    "対象を自分で読めなかった場合（取得できない、本文を読み取れない等）は、別の手段で読もうとせず、",
    "extraction.json と fpml_proposal.xml は作らないこと。review_items.md の先頭に「読めなかった」と書き、理由を書くこと。",
    "page はPDFのページ番号（1から）、quote は原文からの連続した抜き出しにすること。"
  )
  if (agent == "claude") {
    target <- if (.is_url_c1(document$source)) {
      paste0("対象は次のURLのPDF。自分で取得して読むこと：", document$source)
    } else {
      file.copy(document$stored_path, file.path(agent_dir, "document.pdf"), overwrite = TRUE)
      "対象はこのフォルダの `document.pdf`。自分で読むこと。"
    }
    writeLines(c(header, target, "このフォルダ以外のファイルを読み書きしないこと。", "", unreadable),
               file.path(agent_dir, "prompt.md"), useBytes = TRUE)
  } else {
    writeLines(c(header,
                 paste0("対象：", sharepoint_item$url,
                        "（URLの場合は公開元が差し替える可能性があるため、読んだ版のページ数が上と一致するか確認し、違えば review_items.md に書く）"),
                 "作成した3つのファイルを、次のフォルダに保存してください。",
                 paste0("保存先：", normalizePath(agent_dir, winslash = "/")), "", unreadable),
               file.path(agent_dir, "prompt_for_copilot.md"), useBytes = TRUE)
  }
  invisible(agent_dir)
}

# Claude Code を非対話モードで実行する（作業フォルダ内のファイルと、対象URLのドメインだけを使う）
run_claude_agent_c1 <- function(agent_dir, source, claude_cmd = "claude", timeout_sec = 1800) {
  # コマンドラインは英語（Windowsのコマンドラインで日本語が化けないように）。詳しい指示は日本語の prompt.md
  prompt <- paste("Read prompt.md in the current folder and follow it exactly.",
                  "Read the target document yourself; if you cannot read it, do not try other means and",
                  "write only review_items.md explaining why.",
                  "Otherwise create extraction.json, fpml_proposal.xml and review_items.md in this folder.")
  tools <- c("Read", "Write")
  if (.is_url_c1(source)) {
    host <- sub("^https?://([^/:?#]+).*$", "\\1", source, ignore.case = TRUE)
    tools <- c(tools, paste0("WebFetch(domain:", host, ")"))   # 対象のドメインだけウェブ取得を許可
  }
  args <- c("-p", shQuote(prompt), "--output-format", "json",
            "--allowedTools", shQuote(tools), "--permission-mode", "acceptEdits")
  started <- Sys.time()
  out <- withr::with_dir(agent_dir, suppressWarnings(
    system2(claude_cmd, args, stdout = TRUE, stderr = TRUE, timeout = timeout_sec)))
  status <- attr(out, "status") %||% 0L
  writeLines(out, file.path(agent_dir, "claude_run_log.json"), useBytes = TRUE)
  version <- tryCatch(system2(claude_cmd, "--version", stdout = TRUE, stderr = TRUE)[1], error = function(e) NA_character_)
  # 実行ログ（JSON）から、費用（API料金換算の目安）・ターン数・エラーの有無・Claude の最終報告を取り出す
  log <- tryCatch(jsonlite::fromJSON(paste(out, collapse = "\n")), error = function(e) list())
  tibble::tibble(agent = "claude", exit_status = status, started_at = format(started, "%Y-%m-%dT%H:%M:%S%z"),
                 seconds = round(as.numeric(difftime(Sys.time(), started, units = "secs"))), cli_version = version,
                 cost_usd = log$total_cost_usd %||% NA_real_, num_turns = log$num_turns %||% NA_integer_,
                 is_error = log$is_error %||% NA, result = log$result %||% NA_character_)
}

# AIの出力を確かめ、Rで条件一覧のExcelとFpMLのドラフトを作る（AIごとのフォルダ内）
collect_agent_outputs_c1 <- function(agent, agent_dir, pdf_dir, xsd_path = Sys.getenv("CURIE_FPML_XSD", "")) {
  paths <- file.path(agent_dir, .agent_files_c1)
  names(paths) <- names(.agent_files_c1)
  present <- rlang::set_names(file.exists(paths), names(paths))
  status <- tibble::tibble(agent = agent, output = names(paths), file = basename(paths), present = present)
  if (!present[["extraction"]]) {
    # AIが読めなかったと報告した場合（review_items.md の先頭）は、その旨を記録する
    review <- if (present[["review"]]) readLines(paths[["review"]], warn = FALSE, encoding = "UTF-8") else character()
    state <- if (any(grepl("読めなかった", utils::head(review, 5)))) {
      paste0("読めなかった（", agent, " の報告。理由は review_items.md）")
    } else if (agent == "copilot") "未取得（Copilotの出力待ち）" else "未取得"
    return(list(status = status, extraction = NULL, state = state))
  }
  read <- tryCatch(read_trs_extractions_c1(paths[["extraction"]]), error = function(e) e)
  if (inherits(read, "error")) {
    return(list(status = status, extraction = NULL, state = paste("抽出JSONを読めない:", conditionMessage(read))))
  }
  verified <- verify_trs_quotes_c1(read$fields, pdf_dir)
  dict <- trs_field_dictionary_c1()
  table <- verified$fields |>
    dplyr::left_join(dict, by = "field") |>
    dplyr::transmute(区分 = .data$section, 項目ID = .data$field, 項目 = .data$label_ja,
                     必須 = dplyr::if_else(.data$required, "必須", "任意"), 値 = .data$value,
                     原文の状態 = value_status_label_c1(.data$value_status), ページ = .data$page,
                     原文の引用 = .data$quote, 引用の実在確認 = .data$quote_check,
                     確信度 = .data$confidence, メモ = .data$note)
  write_xlsx_CURIE(list("条件一覧" = table, "抽出の実行記録" = read$runs, "形式の問題" = read$format_issues),
                   file.path(agent_dir, "extraction_table.xlsx"))
  # Rが抽出値から作るFpMLのドラフト（AIの変換案とは別。比較用）
  values <- rlang::set_names(read$fields$value, read$fields$field)
  r_fpml <- file.path(agent_dir, "fpml_from_extraction_DRAFT.xml")
  xml2::write_xml(build_fpml_trs_c1(values[!is.na(values)], read$fields$document_id[1], "draft"), r_fpml)
  checks <- purrr::map(c(if (present[["fpml"]]) paths[["fpml"]], r_fpml),
                       \(p) validate_fpml_c1(p, xsd_path)) |> purrr::list_rbind()
  list(status = status, extraction = paths[["extraction"]], fields = verified$fields, hashes = verified$hashes,
       fpml_checks = checks, state = "取得済み")
}
