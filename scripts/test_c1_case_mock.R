# c1 モックテスト：PowerShell の読解処理と R の検証を、Copilot・Claude を呼ばずに確かめる。
#   成功、未接続、取得失敗、Copilot が使えないときの Claude 単独、Claude の読解失敗・回答形式不正、原文引用の改ざん、FpML の不一致
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
copilot_json <- demo$extractions[["copilot"]]

# Claude の結果のモック：原文（デモの正解）で確認し、Copilot の読み違い・読み落としを修正した形
make_claude_demo <- function(dir, tamper = NULL) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  truth <- .demo_trs_truth_c1()
  copilot <- read_trs_extraction_c1(copilot_json) |> select(field, copilot_value = value)
  dict <- trs_field_dictionary_c1()
  rows <- truth |>
    left_join(copilot, by = "field") |>
    left_join(select(dict, field, type), by = "field") |>
    mutate(
      leg_id = case_when(startsWith(field, "return.") ~ "return_leg", startsWith(field, "funding.") ~ "interest_leg", .default = "trade"),
      seq = 1L,
      unit = .expected_unit_c1(field, type, "JPY"),
      unit = if_else(value_status == "stated", unit, ""),
      review_status = case_when(value_status != "stated" & value_status == "not_found" ~ "not_found",
                                value_status != "stated" ~ "unresolved",
                                !is.na(copilot_value) & copilot_value == value ~ "confirmed",
                                .default = "corrected"),
      review_note = if_else(review_status == "corrected", paste0("原文 p.", page, " で確認し、Copilot の値を修正"), ""),
      location = ""
    )
  if (!is.null(tamper)) rows <- tamper(rows)
  fields <- purrr::pmap(rows, function(field, value, value_status, page, quote, copilot_value, leg_id, seq, unit,
                                       review_status, review_note, location, ...) {
    list(field = field, leg_id = leg_id, seq = seq, value = if (is.na(value)) NULL else value, unit = unit,
         value_status = value_status, page = if (is.na(page)) NULL else page, location = location,
         quote = if (is.na(quote)) NULL else quote, copilot_value = if (is.na(copilot_value)) NULL else copilot_value,
         review_status = review_status, review_note = review_note)
  })
  jsonlite::write_json(list(document = list(document_id = "DEMO_TRS_confirmation", extracted_by = "claude"), fields = fields),
                       file.path(dir, "reviewed.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")
  values <- rlang::set_names(rows$value, rows$field)
  xml2::write_xml(build_fpml_trs_c1(values[rows$value_status == "stated"], "DEMO_TRS_confirmation", "draft"), file.path(dir, "fpml.xml"))
  writeLines(c("# 要確認事項（デモ：Claude は呼んでいない）", "", "- funding.spread_bp：Copilot 30 → 原文 35 bp（p.2）"),
             file.path(dir, "review_items.md"), useBytes = TRUE)
  dir
}
claude_ok <- make_claude_demo(file.path(fixtures, "claude_ok"))
claude_tampered <- make_claude_demo(file.path(fixtures, "claude_tampered"), function(r) r |>
  mutate(quote = if_else(field == "funding.day_count", "Day Count Fraction: Actual/360", quote)))
claude_fpml_mismatch <- make_claude_demo(file.path(fixtures, "claude_fpml_mismatch"))
xml_text <- readLines(file.path(claude_fpml_mismatch, "fpml.xml"), encoding = "UTF-8")
writeLines(sub("2026-10-06", "2026-10-07", xml_text, fixed = TRUE), file.path(claude_fpml_mismatch, "fpml.xml"), useBytes = TRUE)
claude_unreadable <- file.path(fixtures, "claude_unreadable"); dir.create(claude_unreadable)
writeLines("読めなかった：document.pdf のページを表示できない", file.path(claude_unreadable, "review_items.md"), useBytes = TRUE)
claude_bad <- file.path(fixtures, "claude_bad"); dir.create(claude_bad)
writeLines("{\"fields\": [{\"value\": \"x\"}]}", file.path(claude_bad, "reviewed.json"))
file.copy(file.path(claude_ok, c("fpml.xml", "review_items.md")), claude_bad)
copilot_unreadable <- file.path(fixtures, "copilot_unreadable.md")
writeLines("読めなかった。添付の資料にアクセスできません。", copilot_unreadable, useBytes = TRUE)
copilot_bad <- file.path(fixtures, "copilot_bad.md")
writeLines("条件は次のとおりです：取引日は2026年10月6日です。", copilot_bad, useBytes = TRUE)

# 入力の種類ごとのテスト資料：ローカルHTML（全体を1ページ）、画像だけのPDF（OCR）、公開PDFのURL
inputs <- file.path(root, "inputs")
html <- write_demo_trs_html_c1(inputs)
scanned <- write_demo_trs_scanned_pdf_c1(inputs, demo$pdf)
claude_html <- make_claude_demo(file.path(fixtures, "claude_html"), function(r) r |> mutate(page = if_else(is.na(page), NA_integer_, 1L)))
public_pdf <- "https://www.sc.com/en/uploads/sites/66/content/docs/Total-Return-Swap.pdf?utm_source=chatgpt.com"
public_404 <- "https://www.sc.com/en/uploads/sites/66/content/docs/CURIE-no-such-file.pdf"

pdf <- demo$pdf
fake_sp <- "https://contoso.sharepoint.com/sites/CURIE/Shared Documents/c1/DEMO_TRS_confirmation.pdf"
cases <- tibble::tribble(
  ~name,                 ~url,     ~copilot, ~copilot_file,       ~claude, ~claude_dir,           ~expect_exit, ~expect_state,
  "成功（両方モック）",  pdf,      "demo",   copilot_json,        "demo",  claude_ok,             0L,           "要確認（FpML の公式XSD",
  "SharePoint 未接続（資料を取得できない）", fake_sp, "api", NA,     "demo",  claude_ok,             2L,           "未接続",
  "Copilot 未接続→Claude 単独", pdf, "api", NA,                 "demo",  claude_ok,             0L,           "要確認（Claude単独処理",
  "ローカルHTML",        html,     "demo",   copilot_json,        "demo",  claude_html,           0L,           "要確認（FpML の公式XSD",
  "画像だけのPDF（OCR）", scanned, "demo",   copilot_json,        "demo",  claude_ok,             0L,           "要確認",
  "公開PDFのURL",        public_pdf, "demo", copilot_json,        "demo",  claude_ok,             0L,           "要確認",
  "公開URLが404",        public_404, "demo", copilot_json,        "demo",  claude_ok,             3L,           "取得失敗",
  "資料がない",          "nofile.pdf", "demo", copilot_json,      "demo",  claude_ok,             3L,           "取得失敗",
  "Copilot 読めない→Claude 単独", pdf, "manual", copilot_unreadable, "demo", claude_ok,          0L,           "要確認（Claude単独処理",
  "Copilot 形式不正→Claude 単独", pdf, "manual", copilot_bad,      "demo",  claude_ok,             0L,           "要確認（Claude単独処理",
  "Claude 読めない",     pdf,      "demo",   copilot_json,        "demo",  claude_unreadable,     6L,           "読解失敗",
  "Claude 形式不正",     pdf,      "demo",   copilot_json,        "demo",  claude_bad,            7L,           "回答形式不正",
  "引用の改ざん",        pdf,      "demo",   copilot_json,        "demo",  claude_tampered,       0L,           "要確認",
  "FpML が CSV と違う",  pdf,      "demo",   copilot_json,        "demo",  claude_fpml_mismatch,  0L,           "要確認",
  # 成功の経路（XSD 適合 → FpML を最終版に）。公式XSD の代わりにテスト専用の緩いXSDを使う（公式の検証ではない）
  "XSD 適合（テスト用XSD）", pdf,  "demo",   copilot_json,        "demo",  claude_ok,             0L,           "デモ"
)
# テスト専用の緩いXSD：ルート要素 dataDocument（FpML 5 confirmation の名前空間）があれば適合とする
test_xsd <- file.path(fixtures, "TEST_ONLY_lax_fpml.xsd")
writeLines(c('<?xml version="1.0" encoding="UTF-8"?>',
             '<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" targetNamespace="http://www.fpml.org/FpML-5/confirmation" elementFormDefault="qualified">',
             '  <xs:element name="dataDocument"><xs:complexType><xs:sequence>',
             '    <xs:any minOccurs="0" maxOccurs="unbounded" processContents="skip"/>',
             '  </xs:sequence><xs:anyAttribute processContents="skip"/></xs:complexType></xs:element>',
             '</xs:schema>'), test_xsd)
results <- purrr::pmap(cases, function(name, url, copilot, copilot_file, claude, claude_dir, expect_exit, expect_state) {
  cat("\n==== ", name, " ====\n", sep = "")
  case <- tryCatch(
    workflows_c1_case(url, copilot, claude, case_id = paste0("mock_", sprintf("%02d", match(name, cases$name))),
                      output_root = file.path(root, "cases"),
                      copilot_file = if (is.na(copilot_file)) NULL else copilot_file, claude_demo_dir = claude_dir,
                      xsd_path = if (startsWith(name, "XSD 適合")) test_xsd else ""),
    error = function(e) list(state = paste("R のエラー:", conditionMessage(e)), exit_code = NA_integer_))
  failed <- if (!is.null(case$validation)) paste(case$validation$checks$検証[case$validation$checks$結果 == "不合格"], collapse = "; ") else ""
  tibble::tibble(テスト = name, 終了コード = case$exit_code, 期待コード = expect_exit, 状態 = case$state,
                 期待状態 = expect_state, 読み方 = case$reading %||% "", FpML = basename(case$fpml %||% NA_character_),
                 不合格の検証 = failed,
                 判定 = if (identical(as.integer(case$exit_code), expect_exit) && startsWith(case$state, expect_state)) "合格" else "不合格")
}) |> purrr::list_rbind()

# 最終版の経路：同じ成果物を、Copilot・Claude とも api だったとして検証し直す（モックのままでは最終版にしない）
final_case <- read_c1_case_results(file.path(root, "cases", sprintf("mock_%02d", nrow(cases))))
final_case$manifest$copilot_mode <- "api"; final_case$manifest$claude_mode <- "api"
final_check <- validate_c1_case(final_case, test_xsd)
results <- dplyr::bind_rows(results, tibble::tibble(
  テスト = "最終版の判定（api とみなす）", 終了コード = 0L, 期待コード = 0L, 状態 = final_check$state, 期待状態 = "検証済み",
  読み方 = final_check$reading, FpML = if (isTRUE(final_check$final)) "fpml_final.xml になる" else "ドラフト", 不合格の検証 = "",
  判定 = if (identical(final_check$state, "検証済み") && isTRUE(final_check$final)) "合格" else "不合格"))

cat("\n\n■ モックテストの結果\n")
print(results, n = Inf, width = Inf)
ok <- results$判定 == "合格"
cat("\n", sum(ok), "／", length(ok), " 件が期待どおり\n", sep = "")
if (!all(ok)) { if (interactive()) stop("期待どおりでないテストがあります。", call. = FALSE) else quit(status = 1) }   # RStudio では終了しない
