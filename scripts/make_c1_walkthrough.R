# c1 の動き方の説明PDF（docs/c1_walkthrough.pdf）を作る。
# 架空のデモ契約書（ローカルの PDF・HTML）で実際に3つの経路を実行し、その画面出力と成果物から説明文書を作る。
# Claude はモック（費用なし）。URL からの取得は scripts/test_c1_http_mock.ps1（HTTP 応答のモック）で確かめる。
#   A. 成功（PDF）
#   B. 成功（HTML。全体を1ページとして扱う）
#   C. 取得失敗（資料がない）
# CURIEプロジェクトの最上位フォルダで実行する：Rscript scripts/make_c1_walkthrough.R

if (!file.exists("CURIE.Rproj")) stop("CURIEプロジェクトの最上位フォルダで実行してください。")
source("R/load_CURIE.R", encoding = "UTF-8")
source("scripts/demo_data/demo_trs_terms.R", encoding = "UTF-8")
suppressPackageStartupMessages(library(dplyr))

work <- file.path("output", "c1_trs", "walkthrough")
unlink(work, recursive = TRUE)
dir.create(work, recursive = TRUE)
demo <- create_demo_trs_terms_c1(file.path("data", "demo_c1_trs"))
claude_demo <- write_demo_claude_review_c1(file.path(work, "claude_demo"))
claude_html <- write_demo_claude_review_c1(file.path(work, "claude_html"), function(r) r |> mutate(page = if_else(is.na(page), NA_integer_, 1L)))
html <- write_demo_trs_html_c1(work)
missing_file <- file.path(demo$pdf_dir, "DEMO_TRS_missing.pdf")

# 1. 3つの経路を実行し、画面出力と案件フォルダを受け取る
rscript <- file.path(R.home("bin"), "Rscript.exe")
run_case <- function(src, demo_dir) {
  out <- system2(rscript, c("scripts/run_c1_document.R", shQuote(src, type = "cmd"), "--claude=demo",
                            paste0("--claude-demo-dir=", demo_dir)), stdout = TRUE, stderr = TRUE)
  dir <- sub("^案件フォルダ：", "", grep("^案件フォルダ：", out, value = TRUE)[1])
  list(console = out, dir = dir, results = read_c1_case_results(dir),
       r = jsonlite::fromJSON(file.path(dir, "r_validation.json")))
}
cases <- list(A = run_case(demo$pdf, claude_demo), B = run_case(html, claude_html), C = run_case(missing_file, claude_demo))
# URL からの取得は HTTP 応答のモックで確かめる（ネットワークにつながない）
http_test <- system2(Sys.which("pwsh"), c("-NoProfile", "-File", "scripts/test_c1_http_mock.ps1"), stdout = TRUE, stderr = TRUE)
http_test <- http_test[nzchar(trimws(http_test))]

# 2. 説明文書（Quarto → Typst → PDF）
esc <- function(x) gsub("|", "\\|", x, fixed = TRUE)
md_table <- function(df) {
  df <- dplyr::mutate(df, dplyr::across(dplyr::everything(), \(x) esc(dplyr::coalesce(as.character(x), ""))))
  c(paste0("| ", paste(names(df), collapse = " | "), " |"), paste0("|", strrep("---|", ncol(df))),
    purrr::pmap_chr(df, \(...) paste0("| ", paste(c(...), collapse = " | "), " |")))
}
# 画面出力のうち、処理の段階を示す行（PowerShell のログ）と結果の行
log_lines <- function(case) {
  x <- grep("\\[c1-ps\\]|\\[c1\\]|^状態：|^段階：|^取得：|^JSON：|^FpML：|^■ トータル", case$console, value = TRUE)
  x <- gsub("C:\\\\AnalyticFin\\\\Projects\\\\CURIE\\\\|C:/AnalyticFin/Projects/CURIE/", "", x)
  substr(x, 1, 160)
}
checks_table <- function(case) {
  md_table(tibble::as_tibble(case$r$checks) |> dplyr::transmute(区分, 検証, 結果, 詳細 = substr(詳細, 1, 60)))
}
key <- c("trade.product_type", "trade.trade_date", "trade.effective_date", "trade.termination_date",
         "return.underlyer_name", "funding.floating_rate_index", "funding.spread_bp", "funding.lookback_days",
         "funding.fixing_offset_days", "funding.fixing_calendar", "funding.payment_calendar")
terms_table <- function(case) {
  case$results$conditions |>
    dplyr::filter(.data$item_id %in% key) |>
    dplyr::arrange(match(.data$item_id, key)) |>
    dplyr::left_join(dplyr::select(trs_field_dictionary_c1(), item_id = "field", 項目 = "label_ja"), by = "item_id") |>
    dplyr::transmute(項目 = sub("（.*）$", "", 項目), 値 = .data$value, 単位 = .data$unit, ページ = .data$source_page,
                     原文の引用 = substr(.data$quote, 1, 40), 確認状態 = .data$review_status) |>
    md_table()
}
json_head <- readLines(file.path(cases$A$dir, "result", "conditions.json"), encoding = "UTF-8")
json_start <- grep('"conditions"', json_head)[1]
json_head <- substr(json_head[json_start + 0:15], 1, 120)
fpml_head <- readLines(file.path(cases$A$dir, "result", "fpml_DRAFT.xml"), n = 18, encoding = "UTF-8")
tree <- c("output/c1_trs/cases/<案件ID>/",
          "  result/conditions.json   成果物：条件（1項目1件。leg_id と seq で反復を区別）、資料・取得の記録、要確認事項",
          "  result/fpml_DRAFT.xml    成果物：FpML（検証済みなら fpml_final.xml）",
          "  manifest.json            実行の記録（段階ごとの状態・終了コード・エラー）",
          "  r_validation.json        R の検証結果",
          "  source/original.pdf|htm  取得した原資料（そのまま）。acquisition.json に取得元・取得日時・HTTP・SHA-256",
          "  source/reading_*         読解用データ（ページごとの本文。必要なら OCR）",
          "  agents/claude/           Claude への依頼文・読解結果（reviewed.json）・FpML 案・実行記録",
          "  logs/run_log.jsonl       段階ごとのログ")

qmd <- c(
  "---",
  "format:",
  "  typst:",
  "    papersize: a4",
  "    margin: {x: 1.6cm, y: 1.6cm}",
  "    fontsize: 9.5pt",
  "    toc: false",
  "---",
  "",
  "```{=typst}",
  "#set text(font: (\"Cambria\", \"MS UI Gothic\"), lang: \"ja\")",
  "#set par(justify: false)",
  "#show raw: set text(font: (\"Consolas\", \"MS UI Gothic\"), size: 7pt)",
  "#show table: set text(size: 7.5pt)",
  "#align(center)[#text(size: 17pt, weight: \"bold\")[CURIE c1：TRS 確認書の読解 ― 動き方]]",
  paste0("#align(center)[#text(size: 10pt)[架空のデモ資料で実行した記録（", format(Sys.Date()), "）]]"),
  "```",
  "",
  "::: {.callout-note}",
  "この文書の資料・値はすべて架空のデモです。入力はローカルのデモ資料（PDF・HTML）で、Claude はモック（費用なし）で動かしました。",
  "URL からの取得は HTTP 応答のモックで確かめています（9 章）。画面の記録と表は、そのときの実際の出力です。",
  ":::",
  "",
  "## 1. 何をするか",
  "",
  "TRS（トータル・リターン・スワップ）の確認書・ターム条件を Claude が読み、参照資産・資金調達の参照金利・フィキシング・",
  "カレンダーなどの条件を、根拠（ページ・位置・原文の引用）つきの **JSON** と **FpML** にします。価格計算はしません。",
  "推測で条件を埋めず、空欄・選択肢・記載なしは、そのまま区別して記録します。",
  "",
  md_table(tibble::tribble(
    ~担当, ~処理,
    "PowerShell（R/c1_trs_terms/PS/）", "資料の取得と記録、読解用データ（本文・OCR）、Claude の読解・FpML 作成、JSON・FpML の保存",
    "R", "入口（URL・ファイルの受付）、終了コード・成果物の受取、条件・根拠・版・FpML（XML構文／公式XSD／条件との一致）の検証、表示"
  )),
  "",
  "## 2. 処理の流れ",
  "",
  "1. **入力**：RStudio で `source(\"scripts/run_c1_document.R\", encoding = \"UTF-8\")` を実行し、「資料のURL（またはファイルのパス）を貼ってください:」に貼る。Box の資料は Box Drive の同期フォルダのパスを貼る",
  "2. **資料の取得**：原資料をそのまま保存し、取得元・取得日時・HTTP・Content-Type・SHA-256 を記録。形式は中身・Content-Type・拡張子で判定。取得できなければ **取得失敗** で止まる（アクセス制限は回避しない）",
  "3. **読解用データ**：PDF はページごとの本文、文字のないページは Windows 標準の OCR、HTML は本文（全体を1ページ）。R の引用照合に使う",
  "4. **Claude**：原資料そのもの（PDF・HTML）を読み、全項目の値・根拠・未確定事項と FpML を作る",
  "5. **保存**：`result/conditions.json`（条件・資料の記録・要確認事項）と FpML",
  "6. **R の検証**：すべて合格し、必須項目がそろい、公式 XSD にも適合したときだけ **検証済み**（`fpml_final.xml`）。それ以外は **要確認**（`fpml_DRAFT.xml`）",
  "",
  "## 3. 状態と終了コード",
  "",
  md_table(tibble::tribble(
    ~状態, ~"終了コード", ~意味,
    "検証済み", "0", "R の検証に合格し、必須項目がそろい、公式 XSD に適合。FpML は fpml_final.xml",
    "要確認", "0", "成果物はあるが、不合格・未確定・XSD 未検証のどれかがある。FpML は fpml_DRAFT.xml",
    "取得失敗", "3", "原資料を取得できない（URL がない、アクセス拒否、ファイルがないなど）",
    "読解失敗／回答形式不正", "6／7", "Claude が原資料を読めない、または結果の形式が不正",
    "読解用データ作成失敗", "8", "本文を抜き出せず、OCR でも読めない"
  )),
  "",
  "## 4. 実行例 A：PDF",
  "",
  paste0("入力：`", demo$pdf, "`（Claude はモック）"),
  "",
  "```", log_lines(cases$A), "```",
  "",
  "**主な条件**",
  "",
  terms_table(cases$A),
  "",
  "**R の検証**",
  "",
  checks_table(cases$A),
  "",
  "## 5. 実行例 B：HTML",
  "",
  "同じ内容の HTML を入力にした。HTML は全体を1ページとして扱い、根拠の位置には条項名を書く。",
  "",
  "```", log_lines(cases$B), "```",
  "",
  "## 6. 実行例 C：取得失敗",
  "",
  "存在しないファイルを指定した。原資料を取得できないため、Claude には進まずに止まる（URL の 404・403 も同じ扱い。9 章）。",
  "",
  "```", log_lines(cases$C), "```",
  "",
  "## 7. 成果物",
  "",
  "```", tree, "```",
  "",
  "**conditions.json の条件の先頭**（1項目1件）",
  "",
  "```json", json_head, "```",
  "",
  "**FpML の先頭**（ドラフト。未確認・未出力の項目は XML コメントに列挙する）",
  "",
  "```xml", fpml_head, "```",
  "",
  "## 8. いまの制約",
  "",
  "- **FpML の公式 XSD**：未設定（`CURIE_FPML_XSD`）。設定するまで「検証済み」にならない",
  "- **Aladdin 用 JSON**：仕様の提示前のため作らない",
  "- **公開元の制限**：SEC などは名前と連絡先を含む User-Agent が必要（`CURIE_HTTP_USER_AGENT`）。アクセス制限は回避しない",
  "- **PDF を Claude が読む**には poppler（pdftoppm）が必要",
  "",
  "## 9. URL からの取得（HTTP 応答のモック）",
  "",
  "`pwsh -File scripts/test_c1_http_mock.ps1`：ネットワークにつながず、HTTP の応答を差し替えて取得処理を確かめた結果。",
  "",
  "```", http_test, "```"
)
qmd_path <- file.path("docs", "c1_walkthrough.qmd")
writeLines(qmd, qmd_path, useBytes = TRUE)
status <- system2("quarto", c("render", qmd_path, "--to", "typst"), stdout = TRUE, stderr = TRUE)
pdf <- file.path("docs", "c1_walkthrough.pdf")
if (!file.exists(pdf)) stop("PDF を作れませんでした:\n", paste(status, collapse = "\n"), call. = FALSE)
cat("作成：", pdf, "（", pdftools::pdf_info(pdf)$pages, " ページ）\n", sep = "")
purrr::iwalk(cases, \(x, k) cat(k, "：", x$r$state, "\n", sep = ""))
