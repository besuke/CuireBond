# c1 モックテスト：R → Claude Code → R の処理を、Claude を呼ばずに確かめる（ネットワークにもつながない）。
#   取得：ローカル PDF・HTML、画像だけの PDF（本文なし）、ファイルがない、URL の 200／403／404（HTTP 応答のモック）、
#         Content-Type と中身の食い違い、HTML の Windows-1252 と &#147;、追跡用パラメータの除去
#   Claude：失敗（終了コード）、タイムアウト、古い成果物を使わない、読めなかった、回答形式不正
#   検証：引用の改ざん、記載ありなのに引用なし、FpML の値の違い・部分一致・別レッグの値、公式XSD 適合時の最終版
# CURIEプロジェクトの最上位フォルダで実行する。データはすべて架空（scripts/demo_data/demo_trs_terms.R）。

if (!file.exists("CURIE.Rproj")) stop("CURIEプロジェクトの最上位フォルダで実行してください。")
source("R/load_CURIE.R", encoding = "UTF-8")
source("scripts/demo_data/demo_trs_terms.R", encoding = "UTF-8")
suppressPackageStartupMessages(library(dplyr))

root <- file.path("output", "c1_trs", "mock_tests")
unlink(root, recursive = TRUE)
fixtures <- file.path(root, "fixtures")
dir.create(fixtures, recursive = TRUE, showWarnings = FALSE)
demo <- create_demo_trs_terms_c1(file.path("data", "demo_c1_trs"))
pdf <- demo$pdf
rscript <- file.path(R.home("bin"), "Rscript.exe")

# ---- Claude の結果のモック -------------------------------------------------------------------
fx <- function(name, tamper = NULL) write_demo_claude_review_c1(file.path(fixtures, name), tamper)
claude_ok <- fx("claude_ok")
claude_html <- fx("claude_html", function(r) r |> mutate(page = if_else(is.na(page), NA_integer_, 1L)))
claude_tampered <- fx("claude_tampered", function(r) r |> mutate(quote = if_else(field == "funding.day_count", "Day Count Fraction: Actual/360", quote)))
claude_noquote <- fx("claude_noquote", function(r) r |> mutate(quote = if_else(field == "funding.spread_bp", NA_character_, quote)))
edit_fpml <- function(name, from, to) {
  d <- fx(name); p <- file.path(d, "fpml.xml")
  writeLines(sub(from, to, readLines(p, encoding = "UTF-8"), fixed = TRUE), p, useBytes = TRUE); d
}
claude_fpml_date <- edit_fpml("claude_fpml_date", "<tradeDate>2026-10-06</tradeDate>", "<tradeDate>2026-10-07</tradeDate>")
claude_fpml_partial <- edit_fpml("claude_fpml_partial", ">JPY-TONA-OIS Compound<", ">JPY-TONA-OIS Compound Averaged<")
# 別レッグの値：interestLeg のスプレッドを消し、returnLeg に同じ値を置く（別レッグの値では合格にしない）
claude_fpml_otherleg <- fx("claude_fpml_otherleg")
local({
  p <- file.path(claude_fpml_otherleg, "fpml.xml"); doc <- xml2::read_xml(p)
  ss <- xml2::xml_find_first(doc, "//*[local-name()='spreadSchedule']"); xml2::xml_remove(ss)
  rl <- xml2::xml_find_first(doc, "//*[local-name()='returnLeg']")
  xml2::xml_add_child(xml2::xml_add_child(rl, "spreadSchedule"), "initialValue", "0.0035")
  xml2::write_xml(doc, p)
})
# 参照の形：interestLeg の支払日を「returnLegPaymentDates と同じ」とだけ書き、営業日調整・カレンダーは参照先に置く
# （href が存在しない id を指す場合は、たどれないので未検証のまま）
fpml_ref <- function(name, href) {
  d <- fx(name); p <- file.path(d, "fpml.xml"); doc <- xml2::read_xml(p)
  ilp <- xml2::xml_find_first(doc, "//*[local-name()='interestLegPaymentDates']")
  xml2::xml_remove(xml2::xml_children(ilp))
  rd <- xml2::xml_add_child(ilp, "relativeDates")
  xml2::xml_add_child(rd, "periodMultiplier", "0"); xml2::xml_add_child(rd, "period", "D")
  xml2::xml_add_child(rd, "dateRelativeTo", href = href)
  rl <- xml2::xml_find_first(doc, "//*[local-name()='returnLeg']")
  rel <- xml2::xml_add_child(xml2::xml_add_child(xml2::xml_add_child(rl, "paymentDates", id = "returnLegPaymentDates"), "paymentDatesInterim"), "relativeDates")
  xml2::xml_add_child(rel, "businessDayConvention", "MODFOLLOWING")
  xml2::xml_add_child(xml2::xml_add_child(rel, "businessCenters"), "businessCenter", "JPTO")
  xml2::write_xml(doc, p); d
}
claude_fpml_ref <- fpml_ref("claude_fpml_ref", "returnLegPaymentDates")
claude_fpml_badref <- fpml_ref("claude_fpml_badref", "noSuchId")
claude_unreadable <- file.path(fixtures, "claude_unreadable"); dir.create(claude_unreadable)
writeLines("読めなかった：document.pdf のページを表示できない", file.path(claude_unreadable, "review_items.md"), useBytes = TRUE)
claude_bad <- file.path(fixtures, "claude_bad"); dir.create(claude_bad)
writeLines("{\"fields\": [{\"value\": \"x\"}]}", file.path(claude_bad, "reviewed.json"))
file.copy(file.path(claude_ok, c("fpml.xml", "review_items.md")), claude_bad)

# ---- 入力資料 ---------------------------------------------------------------------------------
inputs <- file.path(root, "inputs")
html <- write_demo_trs_html_c1(inputs)
scanned <- write_demo_trs_scanned_pdf_c1(inputs, pdf)
# 文字コードの宣言がない Windows-1252 の HTML（“ ” を含む）
html_1252 <- file.path(inputs, "DEMO_TRS_1252.htm")
writeBin(iconv(paste(c(sub("<meta charset=\"utf-8\">", "", readLines(html, encoding = "UTF-8"), fixed = TRUE), "<p>Note: the “Fund”</p>"), collapse = "\n"),
               "UTF-8", "windows-1252", toRaw = TRUE)[[1]], html_1252)
# HTTP 応答のモック：URL の末尾で状態コード・Content-Type・本文を決める（ネットワークにつながない）
last_ua <- NULL
mock_fetcher <- function(url, path, user_agent) {
  last_ua <<- user_agent
  key <- basename(sub("[?#].*$", "", url))
  r <- switch(key,
              "ok.pdf"       = list(200L, "application/pdf", readBin(pdf, "raw", file.size(pdf))),
              "mislabel.bin" = list(200L, "text/html", readBin(pdf, "raw", file.size(pdf))),
              "page.htm"     = list(200L, "text/html", readBin(html_1252, "raw", file.size(html_1252))),
              "denied.htm"   = list(403L, "text/html", charToRaw("Forbidden")),
              list(404L, "text/html", charToRaw("Not Found")))
  writeBin(r[[3]], path)
  list(status = r[[1]], content_type = r[[2]], final_url = url)
}

# テスト専用の緩いXSD：ルート要素 dataDocument（FpML 5 confirmation の名前空間）があれば適合とする（公式の検証ではない）
test_xsd <- file.path(fixtures, "TEST_ONLY_lax_fpml.xsd")
writeLines(c('<?xml version="1.0" encoding="UTF-8"?>',
             '<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" targetNamespace="http://www.fpml.org/FpML-5/confirmation" elementFormDefault="qualified">',
             '  <xs:element name="dataDocument"><xs:complexType><xs:sequence>',
             '    <xs:any minOccurs="0" maxOccurs="unbounded" processContents="skip"/>',
             '  </xs:sequence><xs:anyAttribute processContents="skip"/></xs:complexType></xs:element>',
             '</xs:schema>'), test_xsd)

# ---- 案件の流れのテスト ------------------------------------------------------------------------
fail_cmd <- list(cmd = rscript, args = c("-e", "quit(status = 1)"))
slow_cmd <- list(cmd = rscript, args = c("-e", "Sys.sleep(60)"))
cases <- tibble::tribble(
  ~name,                         ~src,                                      ~mode,  ~demo_dir,            ~run,     ~xsd,     ~expect,
  "成功（ローカルPDF）",         pdf,                                       "demo", claude_ok,            NULL,     "",       "要確認（公式XSD",
  "ローカルHTML",                html,                                      "demo", claude_html,          NULL,     "",       "要確認（公式XSD",
  "画像だけのPDF（本文なし）",        scanned,                                   "demo", claude_ok,            NULL,     "",       "要確認（公式XSD：未実施（XSD未設定）、原文根拠：要確認",
  "URL 200（PDF）",              "https://mock.example/ok.pdf?utm_source=x", "demo", claude_ok,           NULL,     "",       "要確認（公式XSD",
  "URL 200（中身は PDF、型は別）", "https://mock.example/mislabel.bin",     "demo", claude_ok,            NULL,     "",       "要確認（公式XSD",
  "URL 200（HTML・Windows-1252）", "https://mock.example/page.htm",         "demo", claude_html,          NULL,     "",       "要確認（公式XSD",
  "URL 403",                     "https://mock.example/denied.htm",         "demo", claude_ok,            NULL,     "",       "取得失敗",
  "URL 404",                     "https://mock.example/missing.pdf",        "demo", claude_ok,            NULL,     "",       "取得失敗",
  "ファイルがない",              "nofile.pdf",                              "demo", claude_ok,            NULL,     "",       "取得失敗",
  "Claude 失敗（終了コード）",   pdf,                                       "api",  NULL,                 fail_cmd, "",       "読解失敗",
  "Claude タイムアウト",         pdf,                                       "api",  NULL,                 slow_cmd, "",       "読解失敗",
  "Claude 読めなかった",         pdf,                                       "demo", claude_unreadable,    NULL,     "",       "読解失敗",
  "Claude 回答形式不正",         pdf,                                       "demo", claude_bad,           NULL,     "",       "回答形式不正",
  "引用の改ざん",                pdf,                                       "demo", claude_tampered,      NULL,     "",       "要確認（公式XSD：未実施（XSD未設定）、原文根拠：不合格",
  "記載ありなのに引用なし",      pdf,                                       "demo", claude_noquote,       NULL,     "",       "要確認（公式XSD：未実施（XSD未設定）、原文根拠：要確認",
  "FpML の値が違う",             pdf,                                       "demo", claude_fpml_date,     NULL,     "",       "要確認（公式XSD：未実施（XSD未設定）、条件JSONとの整合：不合格",
  "FpML の部分一致",             pdf,                                       "demo", claude_fpml_partial,  NULL,     "",       "要確認（公式XSD：未実施（XSD未設定）、条件JSONとの整合：不合格",
  "FpML の別レッグの値",         pdf,                                       "demo", claude_fpml_otherleg, NULL,     "",       "要確認（公式XSD：未実施（XSD未設定）、条件JSONとの整合：未検証",
  "FpML の参照（dateRelativeTo）", pdf,                                     "demo", claude_fpml_ref,      NULL,     "",       "要確認（公式XSD：未実施（XSD未設定）、条件JSONとの整合：未検証",
  "FpML の参照先がない",         pdf,                                       "demo", claude_fpml_badref,   NULL,     "",       "要確認（公式XSD：未実施（XSD未設定）、条件JSONとの整合：未検証",
  "XSD 適合（テスト用XSD）",     pdf,                                       "demo", claude_ok,            NULL,     test_xsd, "デモ"
)
results <- purrr::pmap(cases, function(name, src, mode, demo_dir, run, xsd, expect) {
  cat("\n==== ", name, " ====\n", sep = "")
  case <- tryCatch(
    workflows_c1_case(src, mode, case_id = sprintf("mock_%02d", match(name, cases$name)), output_root = file.path(root, "cases"),
                      claude_demo_dir = demo_dir, xsd_path = xsd, fetcher = mock_fetcher, timeout_sec = 3,
                      claude_cmd = run$cmd %||% NA_character_, claude_args = run$args),
    error = function(e) list(state = paste("R のエラー:", conditionMessage(e))))
  tibble::tibble(テスト = name, 状態 = case$state, 期待 = expect, FpML = basename(case$fpml %||% NA_character_),
                 理由 = substr(case$results$failure$message %||% "", 1, 70),
                 判定 = if (startsWith(case$state, expect)) "合格" else "不合格")
}) |> purrr::list_rbind()

# ---- 個別の確認 ---------------------------------------------------------------------------------
check <- function(name, ok) tibble::tibble(テスト = name, 状態 = if (isTRUE(ok)) "期待どおり" else "期待と違う", 期待 = "期待どおり",
                                           FpML = NA_character_, 理由 = "", 判定 = if (isTRUE(ok)) "合格" else "不合格")
man <- function(i) jsonlite::fromJSON(file.path(root, "cases", sprintf("mock_%02d", i), "manifest.json"))
reading <- function(i) jsonlite::fromJSON(file.path(root, "cases", sprintf("mock_%02d", i), "source", "reading_pages.json"))
i_of <- function(name) match(name, cases$name)
payment_rows <- function(i) {
  v <- jsonlite::fromJSON(file.path(root, "cases", sprintf("mock_%02d", i), "r_validation.json"))$checks
  v <- v[grepl("^条件との整合：funding.payment", v$検証), ]
  transform(v, item = sub("^条件との整合：", "", v$検証))
}
# 古い成果物：作業フォルダに前回の reviewed.json を置いたまま失敗させても、使わない
stale_dir <- file.path(root, "stale"); dir.create(stale_dir)
writeLines("{\"fields\": []}", file.path(stale_dir, "reviewed.json"))
stale <- run_claude_c1(stale_dir, list(kind = "html"), cmd = rscript,
                       args = c("-e", "writeLines('{}', 'reviewed.json'); quit(status = 2)"), timeout_sec = 10)
results <- dplyr::bind_rows(results,
  check("追跡用パラメータを除いて取得元を記録", identical(man(i_of("URL 200（PDF）"))$source, "https://mock.example/ok.pdf")),
  check("User-Agent を CURIE_HTTP_USER_AGENT から付ける", identical(last_ua, Sys.getenv("CURIE_HTTP_USER_AGENT", "CURIE-c1 (TRS terms reader)")) || nzchar(last_ua)),
  check("中身と Content-Type の食い違いを記録", nzchar(man(i_of("URL 200（中身は PDF、型は別）"))$acquisition$format$note)),
  check("HTML の文字コードを記録（Windows-1252）", identical(man(i_of("URL 200（HTML・Windows-1252）"))$acquisition$encoding, "windows-1252")),
  check("403 の理由に「回避しない」を表示", grepl("HTTP 403.*回避はしません", man(i_of("URL 403"))$failure$message)),
  check("画像だけのPDF を本文なしとして記録（止めない）", all(reading(i_of("画像だけのPDF（本文なし）"))$method == "no_text")),
  check("タイムアウトで Claude を止める", isTRUE(man(i_of("Claude タイムアウト"))$claude$timed_out)),
  check("失敗時に古い・途中の成果物を使わない", stale$state == "failed" && !file.exists(file.path(stale_dir, "reviewed.json"))),
  check("&#147; を “ として読む", grepl("“Fund”", html_text_c1(local({ p <- tempfile(fileext = ".htm"); writeBin(charToRaw("<html><body>the &#147;Fund&#148;</body></html>"), p); p })))),
  check("参照をたどって支払カレンダー・営業日調整が合格（たどった参照を記録）", local({
    x <- payment_rows(i_of("FpML の参照（dateRelativeTo）"))
    all(x$結果[x$item %in% c("funding.payment_calendar", "funding.payment_business_day_convention")] == "合格") &&
      all(grepl("returnLegPaymentDates", x$詳細[x$item %in% c("funding.payment_calendar", "funding.payment_business_day_convention")]))
  })),
  check("参照先の id がなければ支払カレンダー・営業日調整は未検証", local({
    x <- payment_rows(i_of("FpML の参照先がない"))
    all(x$結果[x$item %in% c("funding.payment_calendar", "funding.payment_business_day_convention")] == "未検証")
  })),
  # Aladdin 送信用 JSON（下書き。送信しない）
  check("Aladdin：対応表なしでは下書きと未確定を保存し、向き（pay/rec）を未確定にする", local({
    d <- file.path(root, "cases", sprintf("mock_%02d", i_of("成功（ローカルPDF）")), "result")
    u <- jsonlite::fromJSON(file.path(d, "aladdin_unresolved.json"))
    file.exists(file.path(d, "aladdin_request_DRAFT.json")) && any(grepl("our_party_names", u$unresolved$理由))
  })),
  check("Aladdin：対応表（テスト用の架空コード）があれば、日付・スプレッド・ルックバック・カレンダーを正しいレッグに入れる", local({
    cond <- validate_c1_case(read_c1_case_results(file.path(root, "cases", sprintf("mock_%02d", i_of("成功（ローカルPDF）")))))$conditions
    m <- list(our_party_names = list("CURIE Demo Fund"), spread_unit = "decimal", calendar_codes = list(JPTO = "TEST-TOKYO"))
    p <- build_aladdin_trs_payload_c1(cond, m)$payload
    identical(p$swapInfo$recLeg$startDate, "2026-10-08") && identical(p$swapInfo$payLeg$spread, 0.0035) &&
      identical(p$swapInfo$payLegPaymentReset$lookback, 5L) &&
      identical(unlist(p$swapInfo$payLegPaymentReset$paymentFrequencyCalendar$calendarCodes), "TEST-TOKYO") &&
      is.null(p$swapInfo$recLeg$spread)
  }))
)

# 最終版の経路：同じ成果物を、Claude が api だったとして検証し直す（モックのままでは最終版にしない）
final_case <- read_c1_case_results(file.path(root, "cases", sprintf("mock_%02d", i_of("XSD 適合（テスト用XSD）"))))
final_case$manifest$claude_mode <- "api"
final_check <- validate_c1_case(final_case, test_xsd)
results <- dplyr::bind_rows(results, tibble::tibble(
  テスト = "最終版の判定（api とみなす）", 状態 = final_check$state, 期待 = "検証済み",
  FpML = if (isTRUE(final_check$final)) "fpml_final.xml になる" else "ドラフト", 理由 = "",
  判定 = if (identical(final_check$state, "検証済み") && isTRUE(final_check$final)) "合格" else "不合格"))

cat("\n\n■ モックテストの結果\n")
print(results, n = Inf, width = Inf)
ok <- results$判定 == "合格"
cat("\n", sum(ok), "／", length(ok), " 件が期待どおり\n", sep = "")
if (!all(ok)) { if (interactive()) stop("期待どおりでないテストがあります。", call. = FALSE) else quit(status = 1) }   # RStudio では終了しない
