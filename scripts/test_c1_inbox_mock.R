# c1 フォルダー投入型のモックテスト（Claude は呼ばない。ネットワークにもつながない）
#   複数 PDF、重複投入（同じスキャン内・次のスキャン）、同名衝突、1件の失敗、要確認（取引日なし・自社が不明）、
#   保存の失敗（元 PDF を残す）、コピー中（サイズが変わる）、他の処理が取得済み、相手先名の安全化、作業ディレクトリに依存しない、
#   自社の名前（Curie_OUR_PARTY_NAMES）、保存済みの抽出結果からの作り直し（AI を呼ばない）
# コンソールには英語のテスト名と PASS／FAIL だけを出し、詳細は test_results.tsv に保存する。
# Curieプロジェクトの最上位フォルダで実行する。データはすべて架空（scripts/demo_data/demo_trs_terms.R）。

if (!file.exists("Curie.Rproj")) stop("Run this script from the top folder of the Curie project.")
source("R/load_Curie.R", encoding = "UTF-8")
source("scripts/demo_data/demo_trs_terms.R", encoding = "UTF-8")
Sys.unsetenv(c("Curie_OUR_PARTY_NAMES", "Curie_ALADDIN_MAP"))   # 個人用の設定（tmp/.Renviron）に左右されないように
suppressPackageStartupMessages(library(dplyr))

base <- normalizePath(file.path("output", "c1_trs", "inbox_mock_tests"), winslash = "/", mustWork = FALSE)
unlink(base, recursive = TRUE)
fixtures <- file.path(base, "fixtures")
dir.create(fixtures, recursive = TRUE)
demo <- create_demo_trs_terms_c1(file.path("data", "demo_c1_trs"))
map <- list(our_party_names = list("Curie Demo Fund"))   # 自社＝トータル・リターンの受取者（相手先は DEMO Securities Co., Ltd.）

# コンソールに出る行（cat の結果と message のお知らせの両方）を集める
capture_all <- function(expr) {
  notes <- character()
  out <- utils::capture.output(withCallingHandlers(expr, message = function(m) {
    notes <<- c(notes, strsplit(sub("\n$", "", conditionMessage(m)), "\n", fixed = TRUE)[[1]])
    invokeRestart("muffleMessage")
  }))
  c(out, notes)
}

# 処理中の表示（起動メッセージ・進捗・結果）は出さない。テストの結果だけをコンソールに出す
quiet <- function(expr) {
  invisible(utils::capture.output(value <- suppressWarnings(suppressMessages(expr))))
  value
}

# Claude の結果のモック（元のファイル名で選ぶ）。呼ばれた回数を数える（重複のときは呼ばれないこと）
fx <- function(name, tamper = NULL) normalizePath(write_demo_claude_review_c1(file.path(fixtures, name), tamper), winslash = "/")
claude_ok <- fx("ok")
claude_other_cp <- fx("other_cp", \(r) mutate(r, value = if_else(field == "trade.total_return_payer", "Ab<c>:D/E Bank", value)))
claude_no_trade_date <- fx("no_trade_date", \(r) mutate(r, value = if_else(field == "trade.trade_date", "", value),
                                                         value_status = if_else(field == "trade.trade_date", "not_found", value_status),
                                                         review_status = if_else(field == "trade.trade_date", "not_found", review_status)))
calls <- new.env()
calls$n <- 0L
demo_dir <- function(original) {
  calls$n <- calls$n + 1L
  if (grepl("other_cp", original)) claude_other_cp else if (grepl("no_trade_date", original)) claude_no_trade_date else claude_ok
}

# 中身の違う PDF（末尾にコメントを足して SHA-256 を変える。PDF としては同じに読める）
variant <- function(dest, tag) {
  writeBin(c(readBin(demo$pdf, "raw", file.size(demo$pdf)), charToRaw(paste0("\n% ", tag, "\n"))), dest)
  invisible(dest)
}
put <- function(from, to) invisible(file.copy(from, to))
results <- list()
check <- function(name, ok) results[[length(results) + 1L]] <<- tibble::tibble(test = name, pass = isTRUE(ok))

# ---- 1回目のスキャン：6件（正常、同じ内容の別名、同名衝突、相手先の名前に使えない文字、要確認、壊れた PDF） ----------
config <- c1_inbox_config(file.path(base, "box1"), stable_sec = 0)
put(demo$pdf, file.path(config$inbox, "a_confirmation.pdf"))
put(demo$pdf, file.path(config$inbox, "b_same_content.pdf"))                 # a と同じ内容 → 重複
variant(file.path(config$inbox, "c_same_name.pdf"), "c")                     # 別の内容・同じ共通名 → _2
variant(file.path(config$inbox, "d_other_cp.pdf"), "d")
variant(file.path(config$inbox, "e_no_trade_date.pdf"), "e")
writeBin(charToRaw("this is not a pdf"), file.path(config$inbox, "f_broken.pdf"))   # 失敗しても他は続ける
put(demo$pdf, file.path(config$processed, "z_in_processed.pdf"))             # サブフォルダーは監視しない
old_wd <- setwd(tempdir())                                                    # 作業ディレクトリに依存しないこと
scan1 <- quiet(scan_c1_inbox_once(config, claude_mode = "demo", claude_demo_dir = demo_dir, aladdin_map = map))
setwd(old_wd)
h <- scan1$results
st <- function(f) h$status[h$original_name == f]
nm <- function(f) h$saved_name[h$original_name == f]
root_pdfs <- function(cfg) list.files(cfg$inbox, pattern = "\\.pdf$")
console <- capture_all({ c1_inbox_banner(config); print(scan1) })

check("Watched folder is the root itself", config$inbox == normalizePath(file.path(base, "box1"), winslash = "/"))
check("Startup message shows the watched folder as dir:", any(console == paste0("dir:", config$inbox)))
check("Startup message shows workers and CPU cores", any(grepl("^\\[\\*\\]  Workers: 1 \\(sequential\\)\\. CPU cores on this PC: \\d+ physical / \\d+ logical\\.$", console)))
future::plan(future::multisession, workers = 2)
check("Worker count follows the future plan when future_walk is used", .workers_c1(purrr::walk) == 1L && .workers_c1(furrr::future_walk) == 2L)
future::plan(future::sequential)
check("Console output is English only", !any(grepl("[　-ヿ一-鿿！-｠]", console)))
check("All 6 PDFs processed and recorded (PDFs under processed/ ignored)", nrow(h) == 6L && !"z_in_processed.pdf" %in% h$original_name &&
        file.exists(file.path(config$processed, "z_in_processed.pdf")))
check("Only saved PDFs leave the watched folder (failed PDF stays)", identical(root_pdfs(config), "f_broken.pdf"))
check("Saved under the common name", st("a_confirmation.pdf") == "保存済み" && nm("a_confirmation.pdf") == "TRS_20261006_20271008_DEMO_")
expected_files <- paste0("TRS_20261006_20271008_DEMO_", c(".pdf", ".tsv", ".xml", "_payload.json"))
a_dir <- file.path(config$processed, "TRS_20261006_20271008_DEMO_")
check("PDF, TSV, FpML, payload, validation.json and extraction/ saved",
      all(file.exists(file.path(a_dir, c(expected_files, "validation.json", "extraction/reviewed.json")))))
check("Saved PDF has the same content as the original", identical(pdf_sha256_c1(file.path(a_dir, expected_files[1])), pdf_sha256_c1(demo$pdf)))
check("Duplicate in the same scan: moved to duplicates without calling AI", st("b_same_content.pdf") == "重複" &&
        length(list.files(config$duplicates, pattern = "b_same_content")) == 1L)
check("Name collision: not overwritten, saved as _2", st("c_same_name.pdf") == "保存済み" && nm("c_same_name.pdf") == "TRS_20261006_20271008_DEMO__2" &&
        file.exists(file.path(config$processed, "TRS_20261006_20271008_DEMO__2", "TRS_20261006_20271008_DEMO__2.tsv")))
check("Counterparty name: invalid characters replaced in the first 5 characters", nm("d_other_cp.pdf") == "TRS_20261006_20271008_Ab_c_")
check("No trade date: saved for review without guessing", st("e_no_trade_date.pdf") == "要確認" &&
        dir.exists(file.path(config$review, "TRS_UNKNOWN_20271008_DEMO_")) && grepl("trade date", h$reason[h$original_name == "e_no_trade_date.pdf"]))
check("One failure: reason saved in failed/, original PDF left unchanged", st("f_broken.pdf") == "失敗" &&
        length(list.files(config$failed, pattern = "f_broken\\.pdf\\.reason\\.json$")) == 1L &&
        rawToChar(readBin(file.path(config$inbox, "f_broken.pdf"), "raw", 100)) == "this is not a pdf")
check("Claude (mock) called only for the 5 non-duplicates", calls$n == 5L)
check("No work folders or locks left", !length(list.files(config$work)) && !length(list.files(config$locks)))

# TSV・validation.json・payload の中身（同じ抽出・検証済みデータから）
tsv <- read.delim(file.path(a_dir, expected_files[2]), sep = "|", quote = "", fileEncoding = "UTF-8-BOM", check.names = FALSE,
                  colClasses = "character")
check("TSV is long format (item, leg, period, value, unit, quote, result)",
      all(c("項目ID", "レッグ", "期間番号", "値", "単位", "原文根拠", "検証結果", "FpMLとの整合") %in% names(tsv)) &&
        tsv$値[tsv$項目ID == "trade.termination_date"] == "2027-10-08")
v <- jsonlite::read_json(file.path(a_dir, "validation.json"))
check("validation.json has original name, SHA-256 and DRAFT / NOT SENT", v$original_name == "a_confirmation.pdf" &&
        v$sha256 == pdf_sha256_c1(demo$pdf) && v$payload_status == "DRAFT / NOT SENT")
payload <- jsonlite::read_json(file.path(a_dir, expected_files[4]))
check("Payload has the extracted dates", payload$swapInfo$recLeg$endDate == "2027-10-08")

# ---- 2回目のスキャン：同じ PDF を再投入 → 重複（AI を呼ばない） ------------------------------------------------
n_before <- calls$n
put(demo$pdf, file.path(config$inbox, "a_again.pdf"))
scan2 <- quiet(scan_c1_inbox_once(config, claude_mode = "demo", claude_demo_dir = demo_dir, aladdin_map = map))
check("Re-submitted PDF is a duplicate (saved location recorded)", scan2$results$status == "重複" &&
        scan2$results$saved_name == "TRS_20261006_20271008_DEMO_" && calls$n == n_before)
check("Previously failed PDF is on hold (AI not called)", identical(scan2$held, "f_broken.pdf") && calls$n == n_before)
scan2b <- quiet(scan_c1_inbox_once(config, claude_mode = "demo", claude_demo_dir = demo_dir, aladdin_map = map, retry_failed = TRUE))
check("retry_failed = TRUE processes it again", identical(scan2b$results$original_name, "f_broken.pdf") && scan2b$results$status == "失敗")
check("History maps original names to saved names", all(c("a_confirmation.pdf", "c_same_name.pdf") %in%
                                                          dplyr::filter(read_c1_history(config), status == "保存済み")$original_name))

# ---- 自社の名前がない（対応表なし）→ 相手先不明で要確認 → 自社の名前を設定して作り直す（AI を呼ばない） --------------
config3 <- c1_inbox_config(file.path(base, "box3"), stable_sec = 0)
variant(file.path(config3$inbox, "g_no_map.pdf"), "g")
n_before <- calls$n
scan3 <- quiet(scan_c1_inbox_once(config3, claude_mode = "demo", claude_demo_dir = demo_dir, aladdin_map = list()))
review_dir <- file.path(config3$review, "TRS_20261006_20271008_UNKNOWN")
check("Our party unknown: counterparty not guessed, saved for review", scan3$results$status == "要確認" &&
        scan3$results$saved_name == "TRS_20261006_20271008_UNKNOWN" && dir.exists(file.path(review_dir, "extraction")))
n_after_scan <- calls$n
still <- quiet(regenerate_c1_review_cases(config3, aladdin_map = list()))
check("Regeneration without our party keeps the review case as is", dir.exists(review_dir) && !nrow(still))
Sys.setenv(Curie_OUR_PARTY_NAMES = "Other Fund; Curie Demo Fund")
env_map <- read_aladdin_map_c1(path = "")
Sys.unsetenv("Curie_OUR_PARTY_NAMES")
check("Curie_OUR_PARTY_NAMES sets our party names", identical(unlist(env_map$our_party_names), c("Other Fund", "Curie Demo Fund")))
regen <- quiet(regenerate_c1_review_cases(config3, aladdin_map = env_map))
new_dir <- file.path(config3$processed, "TRS_20261006_20271008_DEMO_")
check("Regenerated from saved extraction without calling AI", calls$n == n_after_scan && nrow(regen) == 1L &&
        grepl("AI not called", regen$reason) && regen$status == "保存済み")
check("Regenerated case moved from review to processed with the common name", !dir.exists(review_dir) &&
        all(file.exists(file.path(new_dir, paste0("TRS_20261006_20271008_DEMO_", c(".pdf", ".tsv", ".xml", "_payload.json"))))))
v3 <- jsonlite::read_json(file.path(new_dir, "validation.json"))
check("Regenerated case keeps the original name and is not labelled demo", v3$original_name == "g_no_map.pdf" && !startsWith(v3$state, "デモ"))
p3 <- jsonlite::read_json(file.path(new_dir, "TRS_20261006_20271008_DEMO__payload.json"))
check("Regenerated payload uses our side (return leg is recLeg)", identical(p3$swapInfo$recLeg$endDate, "2027-10-08"))
variant(file.path(config3$inbox, "g_again.pdf"), "g")
dup3 <- quiet(scan_c1_inbox_once(config3, claude_mode = "demo", claude_demo_dir = demo_dir, aladdin_map = env_map))
check("Duplicate after regeneration points to the new saved case", dup3$results$status == "重複" &&
        dup3$results$saved_dir == new_dir && calls$n == n_after_scan)

# 従来の案件フォルダー（output/c1_trs/cases の形）からも作り直せる（元のフォルダーは残す）
old_case <- quiet(workflows_c1_case(demo$pdf, "demo", case_id = "old_case", output_root = file.path(base, "cases"), claude_demo_dir = claude_ok))
config6 <- c1_inbox_config(file.path(base, "box6"), stable_sec = 0)
r6 <- quiet(regenerate_c1_case(old_case$case_dir, config6, aladdin_map = map))
check("Regenerated from a case folder (agents/claude) and the original folder kept", dir.exists(old_case$case_dir) &&
        identical(r6$name, "TRS_20261006_20271008_DEMO_") && file.exists(file.path(r6$dest, "TRS_20261006_20271008_DEMO_.tsv")))

# ---- 保存の失敗：保存先が作れない → 元 PDF を残す ----------------------------------------------------------
blocked <- file.path(base, "box4_blocked")
writeLines("not a folder", blocked)                                           # 保存先がファイルなのでフォルダーを作れない
config4 <- c1_inbox_config(file.path(base, "box4"), processed = blocked, stable_sec = 0)
variant(file.path(config4$inbox, "h_save_fail.pdf"), "h")
scan4 <- quiet(scan_c1_inbox_once(config4, claude_mode = "demo", claude_demo_dir = claude_ok, aladdin_map = map))
check("Save failure: original PDF not deleted or moved, reason recorded", scan4$results$status == "失敗" &&
        grepl("destination", scan4$results$reason) && identical(root_pdfs(config4), "h_save_fail.pdf") &&
        pdf_sha256_c1(file.path(config4$inbox, "h_save_fail.pdf")) == scan4$results$sha256)

# ---- コピー中（サイズが変わっている）PDF は処理しない ---------------------------------------------------------
config5 <- c1_inbox_config(file.path(base, "box5"), stable_sec = 60)
growing <- variant(file.path(config5$inbox, "i_copying.pdf"), "i")
snap <- .inbox_snapshot_c1(config5$inbox)
cat("more", file = growing, append = TRUE)                                    # 前回の一覧からサイズが変わった
scan5 <- quiet(scan_c1_inbox_once(config5, claude_mode = "demo", claude_demo_dir = claude_ok, aladdin_map = map, snapshot = snap))
check("While copying (size or time changing) the PDF waits", !nrow(scan5$results) && scan5$waiting == "i_copying.pdf" && file.exists(growing))
old <- dplyr::mutate(scan5$snapshot, since = since - 61)                      # 61 秒変わらなかったことにする
scan5b <- quiet(scan_c1_inbox_once(config5, claude_mode = "demo", claude_demo_dir = claude_ok, aladdin_map = map, snapshot = old))
check("After copying (unchanged for the set time) the PDF is processed", scan5b$results$status == "保存済み")

# ---- 他の処理（並列ワーカー）が先に取得した PDF は扱わない ------------------------------------------------------
taken <- variant(file.path(config5$inbox, "j_taken.pdf"), "j")
dir.create(file.path(config5$locks, pdf_sha256_c1(taken)))                   # 別のワーカーがロックを取った状態
n_before <- calls$n
busy <- quiet(process_c1_pdf(taken, config5, "demo", demo_dir, map))
check("Locked PDF skipped (AI not called, original untouched)", busy$status == "スキップ" &&
        calls$n == n_before && file.exists(taken) && !"j_taken.pdf" %in% read_c1_history(config5)$original_name)
gone <- quiet(process_c1_pdf(file.path(config5$inbox, "already_done.pdf"), config5, "demo", claude_ok, map))
check("PDF already gone is skipped", gone$status == "スキップ")

# ---- 監視の出来事を RStudio のコンソールに表示する（later で events.jsonl を読む） ------------------------------------
# 1つの式にまとめる（トップレベルの式の合間に later の表示が走り、テストのコンソールに出ないように）
local({
config7 <- c1_inbox_config(file.path(base, "box7"), stable_sec = 0)
w7 <- .watch_paths_c1(config7)
dir.create(w7$lock)                                                           # 監視が動いている状態
follow_out <- capture_all(follow_c1_inbox_watch(config7, interval_sec = 0.1))
variant(file.path(config7$inbox, "k_watched.pdf"), "k")
writeBin(charToRaw("this is not a pdf"), file.path(config7$inbox, "l_broken.pdf"))
quiet(scan_c1_inbox_once(config7, claude_mode = "demo", claude_demo_dir = claude_ok, aladdin_map = map, banner = FALSE))
pump <- function() capture_all(purrr::walk(1:5, \(i) later::run_now(0.3)))
shown <- pump()
unlink(w7$lock, recursive = TRUE)                                             # 監視が終わった
shown_end <- pump()
k_dir <- file.path(config7$processed, "TRS_20261006_20271008_DEMO_")
check("Console follower: detection, start, completion and failure shown in English",
      all(purrr::map_lgl(c("PDF detected. k_watched.pdf", "Processing started. k_watched.pdf", "Processing completed. k_watched.pdf",
                           "Processing failed. l_broken.pdf"), \(p) any(grepl(p, shown, fixed = TRUE)))) &&
        !any(grepl("[　-ヿ一-鿿！-｠]", c(follow_out, shown))))
check("Console follower: completion shows CP, Start, End, Pay leg, Receive leg and dir:",
      all(c("CP:          DEMO Securities Co., Ltd.", "Start:       2026-10-08", "End:         2027-10-08",
            "Pay leg:     JPY-TONA-OIS Compound + 35 bp / Quarterly",
            "Receive leg: DEMO Japan Credit Bond Index / Total Return / Quarterly", paste0("dir:", k_dir)) %in% shown))
check("Console follower: failure shows the reason file as file:", any(grepl("^file:.*l_broken\\.pdf\\.reason\\.json$", shown)))
check("Console follower stops when the watcher finishes", any(grepl("Watcher finished.", shown_end, fixed = TRUE)) &&
        !exists(config7$root, envir = .c1_follow_env))
})

# ---- 相手先名の安全化 ------------------------------------------------------------------------------
check("Safe name part", identical(purrr::map_chr(c("  Example Imperial", "A?B*C", "AB.  x", "Abc D Ltd.", "日本証券株式会社"), .safe_name_part_c1),
                                  c("Examp", "A_B_C", "AB___", "Abc D", "日本証券株")))

all_ok <- report_test_results_Curie(purrr::list_rbind(results), file.path(base, "test_results.tsv"))
if (!all_ok) quit(status = 1)
