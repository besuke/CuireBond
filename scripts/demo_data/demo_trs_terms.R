# c1のデモデータ：記入済みの架空TRSターム条件PDFと、2つのAIの抽出JSON（手作り）。
# すべて架空。PDFは Quarto（Typst）で作る。抽出JSONは実際のAIの出力ではなく、
# 「Claude と Copilot が読んだ結果」に見立てたデモで、Copilot側にわざと読み違いと読み落としを入れる。

# ターム条件の本文（1行1項目。ページ1：取引・参照資産、ページ2：資金調達レッグ）
.demo_trs_lines_c1 <- function() {
  list(
    page1 = c(
      "FICTITIOUS DEMO CONFIRMATION - NOT A REAL TRANSACTION",
      "Total Return Swap Confirmation",
      "Dealer: DEMO Securities Co., Ltd.",
      "Dealer Reference: DEMO-TRS-0001",
      "Trade Date: 2026-10-06",
      "Effective Date: 2026-10-08",
      "Termination Date: 2027-10-08",
      "Notional Amount: JPY 1,000,000,000",
      "Total Return Receiver: CURIE Demo Fund",
      "Total Return Payer: DEMO Securities Co., Ltd.",
      "Calculation Agent: DEMO Securities Co., Ltd.",
      "Definitions: 2021 ISDA Interest Rate Derivatives Definitions",
      "Reference Asset Type: Index",
      "Reference Index: DEMO Japan Credit Bond Index",
      "Index Code: DEMOJCBI",
      "Index Currency: JPY",
      "Return Type: Total Return",
      "Initial Price: 100.00",
      "Valuation Frequency: Quarterly",
      "Valuation Dates: Two Business Days prior to each Payment Date",
      "Valuation Business Days: Tokyo",
      "Final Valuation Date: 2027-10-06",
      "Valuation Time: 15:00 Tokyo time",
      "Income: Coupons of the index constituents are reinvested in the index"
    ),
    page2 = c(
      "Funding Leg",
      "Floating Rate Option: JPY-TONA-OIS Compound",
      "Spread: Plus 0.35% per annum",
      "Day Count Fraction: Actual/365 (Fixed)",
      "Compounding: Compounding",
      "Payment Frequency: Quarterly",
      "Reset Frequency: Quarterly",
      "Reset Dates: The first day of each Calculation Period",
      "Fixing Date: 2 Business Days prior to each Reset Date",
      "Fixing Business Days: Tokyo",
      "Fixing Business Day Convention: Preceding",
      "Lookback: 5 Business Days",
      "Observation Period Shift: Not Applicable",
      "Payment Lag: 2 Business Days",
      "Payment Business Day Convention: Modified Following",
      "Payment Business Days: Tokyo"
    )
  )
}

# 架空のターム条件PDFを作る（Quarto＋Typst）。上位に _quarto.yml がないフォルダで実行すること
write_demo_trs_pdf_c1 <- function(dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  lines <- .demo_trs_lines_c1()
  qmd <- file.path(dir, "DEMO_TRS_confirmation.qmd")
  body <- c("---", "format: typst", "---", "",
            paste0(lines$page1, "\n"), "{{< pagebreak >}}", "", paste0(lines$page2, "\n"))
  writeLines(body, qmd, useBytes = TRUE)
  status <- withr::with_dir(dir, system2("quarto", c("render", basename(qmd), "--to", "typst", "--quiet"),
                                         stdout = TRUE, stderr = TRUE))
  pdf <- file.path(dir, "DEMO_TRS_confirmation.pdf")
  if (!file.exists(pdf)) stop("デモPDFを作れませんでした: ", paste(status, collapse = "\n"), call. = FALSE)
  unlink(qmd)
  pdf
}

# 抽出の正解（Claude役）。page と quote はPDFの本文と同じ
.demo_trs_truth_c1 <- function() {
  tibble::tribble(
    ~field,                                    ~value,                                   ~value_status, ~page, ~quote,
    "trade.product_type",                      "TOTAL_RETURN_SWAP",                      "stated",      1L,    "Total Return Swap Confirmation",
    "trade.dealer",                            "DEMO Securities Co., Ltd.",              "stated",      1L,    "Dealer: DEMO Securities Co., Ltd.",
    "trade.dealer_reference",                  "DEMO-TRS-0001",                          "stated",      1L,    "Dealer Reference: DEMO-TRS-0001",
    "trade.trade_date",                        "2026-10-06",                             "stated",      1L,    "Trade Date: 2026-10-06",
    "trade.effective_date",                    "2026-10-08",                             "stated",      1L,    "Effective Date: 2026-10-08",
    "trade.termination_date",                  "2027-10-08",                             "stated",      1L,    "Termination Date: 2027-10-08",
    "trade.notional_amount",                   "1000000000",                             "stated",      1L,    "Notional Amount: JPY 1,000,000,000",
    "trade.notional_currency",                 "JPY",                                    "stated",      1L,    "Notional Amount: JPY 1,000,000,000",
    "trade.total_return_receiver",             "CURIE Demo Fund",                        "stated",      1L,    "Total Return Receiver: CURIE Demo Fund",
    "trade.total_return_payer",                "DEMO Securities Co., Ltd.",              "stated",      1L,    "Total Return Payer: DEMO Securities Co., Ltd.",
    "trade.calculation_agent",                 "DEMO Securities Co., Ltd.",              "stated",      1L,    "Calculation Agent: DEMO Securities Co., Ltd.",
    "trade.isda_definitions",                  "2021 ISDA Interest Rate Derivatives Definitions", "stated", 1L, "Definitions: 2021 ISDA Interest Rate Derivatives Definitions",
    "trade.fx_initial_spot_rate",              NA,                                       "not_found",   NA,    NA,
    "trade.fx_rate_quotation",                 NA,                                       "not_found",   NA,    NA,
    "trade.fx_hedge",                          NA,                                       "not_found",   NA,    NA,
    "return.underlyer_type",                   "INDEX",                                  "stated",      1L,    "Reference Asset Type: Index",
    "return.underlyer_name",                   "DEMO Japan Credit Bond Index",           "stated",      1L,    "Reference Index: DEMO Japan Credit Bond Index",
    "return.underlyer_id",                     "DEMOJCBI",                               "stated",      1L,    "Index Code: DEMOJCBI",
    "return.underlyer_id_type",                "INDEX_CODE",                             "stated",      1L,    "Index Code: DEMOJCBI",
    "return.underlyer_currency",               "JPY",                                    "stated",      1L,    "Index Currency: JPY",
    "return.underlyer_issuer",                 NA,                                       "not_found",   NA,    NA,
    "return.underlyer_maturity_date",          NA,                                       "not_found",   NA,    NA,
    "return.return_type",                      "TOTAL",                                  "stated",      1L,    "Return Type: Total Return",
    "return.initial_price",                    "100",                                    "stated",      1L,    "Initial Price: 100.00",
    "return.valuation_frequency",              "QUARTERLY",                              "stated",      1L,    "Valuation Frequency: Quarterly",
    "return.valuation_date_rule",              "Two Business Days prior to each Payment Date", "stated", 1L,   "Valuation Dates: Two Business Days prior to each Payment Date",
    "return.valuation_calendar",               "Tokyo",                                  "stated",      1L,    "Valuation Business Days: Tokyo",
    "return.final_valuation_date",             "2027-10-06",                             "stated",      1L,    "Final Valuation Date: 2027-10-06",
    "return.valuation_time",                   "15:00 Tokyo time",                       "stated",      1L,    "Valuation Time: 15:00 Tokyo time",
    "return.income_treatment",                 "Coupons of the index constituents are reinvested in the index", "stated", 1L, "Income: Coupons of the index constituents are reinvested in the index",
    "funding.floating_rate_index",             "JPY-TONA-OIS Compound",                  "stated",      2L,    "Floating Rate Option: JPY-TONA-OIS Compound",
    "funding.index_tenor",                     NA,                                       "not_found",   NA,    NA,
    "funding.spread_bp",                       "35",                                     "stated",      2L,    "Spread: Plus 0.35% per annum",
    "funding.rate_floor_pct",                  NA,                                       "not_found",   NA,    NA,
    "funding.day_count",                       "Actual/365 (Fixed)",                     "stated",      2L,    "Day Count Fraction: Actual/365 (Fixed)",
    "funding.compounding_method",              "COMPOUNDING",                            "stated",      2L,    "Compounding: Compounding",
    "funding.payment_frequency",               "QUARTERLY",                              "stated",      2L,    "Payment Frequency: Quarterly",
    "funding.reset_frequency",                 "QUARTERLY",                              "stated",      2L,    "Reset Frequency: Quarterly",
    "funding.reset_relative_to",               "PERIOD_START",                           "stated",      2L,    "Reset Dates: The first day of each Calculation Period",
    "funding.fixing_offset_days",              "-2",                                     "stated",      2L,    "Fixing Date: 2 Business Days prior to each Reset Date",
    "funding.fixing_calendar",                 "Tokyo",                                  "stated",      2L,    "Fixing Business Days: Tokyo",
    "funding.fixing_business_day_convention",  "Preceding",                              "stated",      2L,    "Fixing Business Day Convention: Preceding",
    "funding.lookback_days",                   "5",                                      "stated",      2L,    "Lookback: 5 Business Days",
    "funding.observation_shift",               "NO",                                     "stated",      2L,    "Observation Period Shift: Not Applicable",
    "funding.payment_lag_days",                "2",                                      "stated",      2L,    "Payment Lag: 2 Business Days",
    "funding.payment_business_day_convention", "Modified Following",                     "stated",      2L,    "Payment Business Day Convention: Modified Following",
    "funding.payment_calendar",                "Tokyo",                                  "stated",      2L,    "Payment Business Days: Tokyo"
  )
}

.write_demo_extraction_c1 <- function(fields, agent, pdf, path) {
  doc <- list(
    document = list(document_id = "DEMO_TRS_0001", file = basename(pdf), dealer = "DEMO Securities Co., Ltd.",
                    pages = pdftools::pdf_info(pdf)$pages, pdf_sha256 = pdf_sha256_c1(pdf),
                    extracted_by = agent, model = paste0("DEMO（", agent, " の出力に見立てた手作りデータ）"),
                    instruction_version = "c1-extract-v1", extracted_at = "2026-10-26T09:00:00+09:00"),
    fields = purrr::pmap(fields, function(field, value, value_status, page, quote, confidence = "high", note = "") {
      list(field = field, value = if (is.na(value)) NULL else value, value_status = value_status,
           page = if (is.na(page)) NULL else page, quote = if (is.na(quote)) NULL else quote,
           confidence = confidence, note = note)
    })
  )
  jsonlite::write_json(doc, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  path
}

# デモ一式を作る：PDF、Claude役とCopilot役の抽出JSON、人による確認結果のCSV
create_demo_trs_terms_c1 <- function(dir) {
  pdf_dir <- file.path(dir, "pdf")
  ext_dir <- file.path(dir, "extractions")
  dir.create(ext_dir, recursive = TRUE, showWarnings = FALSE)
  pdf <- write_demo_trs_pdf_c1(pdf_dir)
  truth <- .demo_trs_truth_c1()
  copilot <- truth |>
    dplyr::mutate(
      # 読み違い：0.35% を 0.30% と読んだ（引用は正しい原文のまま）
      value = dplyr::if_else(.data$field == "funding.spread_bp", "30", .data$value),
      # 読み落とし：ルックバックを見落とした
      value = dplyr::if_else(.data$field == "funding.lookback_days", NA_character_, .data$value),
      value_status = dplyr::if_else(.data$field == "funding.lookback_days", "not_found", .data$value_status),
      quote = dplyr::if_else(.data$field == "funding.lookback_days", NA_character_, .data$quote),
      page = dplyr::if_else(.data$field == "funding.lookback_days", NA_integer_, .data$page)
    )
  paths <- c(
    claude = .write_demo_extraction_c1(truth, "claude", pdf, file.path(ext_dir, "DEMO_TRS_0001__claude.json")),
    copilot = .write_demo_extraction_c1(copilot, "copilot", pdf, file.path(ext_dir, "DEMO_TRS_0001__copilot.json"))
  )
  # 人による確認結果（不一致・片方のみの項目を、PDFを見て確定した記録）
  overrides <- tibble::tribble(
    ~document_id,    ~field,                  ~value, ~reviewer,       ~reviewed_at,                ~reason,
    "DEMO_TRS_0001", "funding.spread_bp",     "35",   "DEMO REVIEWER", "2026-10-26T10:00:00+09:00", "PDF p.2「Spread: Plus 0.35% per annum」を確認",
    "DEMO_TRS_0001", "funding.lookback_days", "5",    "DEMO REVIEWER", "2026-10-26T10:00:00+09:00", "PDF p.2「Lookback: 5 Business Days」を確認"
  )
  overrides_path <- file.path(dir, "DEMO_TRS_0001_overrides.csv")
  readr::write_csv(overrides, overrides_path)
  list(pdf_dir = pdf_dir, pdf = pdf, extractions = paths, overrides = overrides_path)
}

write_demo_trs_html_c1 <- function(dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  lines <- .demo_trs_lines_c1()
  esc <- function(x) gsub("<", "&lt;", gsub("&", "&amp;", x, fixed = TRUE), fixed = TRUE)
  html <- c("<!DOCTYPE html>", "<html><head><meta charset=\"utf-8\"><title>DEMO TRS confirmation</title></head><body>",
            paste0("<p>", esc(c(lines$page1, lines$page2)), "</p>"), "</body></html>")
  path <- file.path(dir, "DEMO_TRS_confirmation.htm")
  writeLines(html, path, useBytes = TRUE)
  path
}

# 文字を持たない（画像だけの）PDF：OCR のテスト用。デモPDFのページを画像にし、Chrome で PDF に印刷する
write_demo_trs_scanned_pdf_c1 <- function(dir, pdf) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  pngs <- pdftools::pdf_convert(pdf, format = "png", dpi = 200,
                                filenames = file.path(dir, sprintf("scan_p%d.png", seq_len(pdftools::pdf_info(pdf)$pages))), verbose = FALSE)
  html <- file.path(dir, "scan.html")
  writeLines(c("<!DOCTYPE html><html><head><style>@page{margin:0} img{width:100%;display:block;page-break-after:always}</style></head><body>",
               paste0("<img src=\"", basename(pngs), "\">"), "</body></html>"), html, useBytes = TRUE)
  out <- file.path(dir, "DEMO_TRS_scanned.pdf")
  html_to_pdf_c1(html, out)
  out
}
