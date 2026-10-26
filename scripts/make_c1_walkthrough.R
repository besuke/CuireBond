# c1 の動き方の説明PDF（docs/c1_walkthrough.pdf）を作る。
# 架空のデモ契約書（ローカルのPDF）で実際に3つの経路を実行し、その画面出力と成果物から説明文書を作る。
# Copilot・Claude はモック（費用なし）。URL からの取得は scripts/test_c1_http_mock.ps1（HTTP 応答のモック）で確かめる。
#   A. Copilot一次読解＋Claude確認
#   B. Claude単独処理（Copilot 未実施：未接続）
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
claude_demo <- write_demo_claude_review_c1(file.path(work, "claude_demo"), demo$extractions[["copilot"]])

# 1. 入力：ローカルのデモPDF（架空）
mock_url <- demo$pdf
missing_file <- file.path(demo$pdf_dir, "DEMO_TRS_missing.pdf")

# 2. 3つの経路を実行し、画面出力と案件フォルダを受け取る
rscript <- file.path(R.home("bin"), "Rscript.exe")
run_case <- function(url, ...) {
  out <- system2(rscript, c("scripts/run_c1_document.R", shQuote(url, type = "cmd"), ...), stdout = TRUE, stderr = TRUE)
  dir <- sub("^案件フォルダ：", "", grep("^案件フォルダ：", out, value = TRUE)[1])
  list(console = out, dir = dir, results = read_c1_case_results(dir),
       r = jsonlite::fromJSON(file.path(dir, "r_validation.json")))
}
copilot_demo <- c(paste0("--copilot-file=", demo$extractions[["copilot"]]))
# URL からの取得は HTTP 応答のモックで確かめる（ネットワークにつながない）
http_test <- system2(Sys.which("pwsh"), c("-NoProfile", "-File", "scripts/test_c1_http_mock.ps1"), stdout = TRUE, stderr = TRUE)
http_test <- http_test[nzchar(trimws(http_test))]
cases <- list(
  A = run_case(mock_url, "--copilot=demo", copilot_demo, "--claude=demo", paste0("--claude-demo-dir=", claude_demo)),
  B = run_case(mock_url, "--copilot=api", "--claude=demo", paste0("--claude-demo-dir=", claude_demo)),
  C = run_case(missing_file, "--copilot=demo", copilot_demo, "--claude=demo",
               paste0("--claude-demo-dir=", claude_demo))
)

# 3. 説明文書（Quarto → Typst → PDF）
esc <- function(x) gsub("|", "\\|", x, fixed = TRUE)
md_table <- function(df) {
  df <- dplyr::mutate(df, dplyr::across(dplyr::everything(), \(x) esc(dplyr::coalesce(as.character(x), ""))))
  c(paste0("| ", paste(names(df), collapse = " | "), " |"), paste0("|", strrep("---|", ncol(df))),
    purrr::pmap_chr(df, \(...) paste0("| ", paste(c(...), collapse = " | "), " |")))
}
# 画面出力のうち、処理の段階を示す行（PowerShell のログ）と結果の行
log_lines <- function(case) {
  x <- grep("\\[c1-ps\\]|\\[c1\\]|^読み方：|^状態：|^段階：|^取得：|^Copilot への|^FpML：|^■ トータル", case$console, value = TRUE)
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
                     Copilotの値 = .data$copilot_value, 確認状態 = .data$review_status) |>
    md_table()
}
csv_head <- readLines(file.path(cases$A$dir, "result", "conditions.csv"), n = 4, encoding = "UTF-8")
csv_head <- sub("^\ufeff", "", substr(csv_head, 1, 150))
fpml_head <- readLines(file.path(cases$A$dir, "result", "fpml_DRAFT.xml"), n = 18, encoding = "UTF-8")
tree <- c("output/c1_trs/cases/<案件ID>/",
          "  manifest.json            実行の記録（段階ごとの状態・終了コード・モード・読み方・エラー）",
          "  r_validation.json        R の検証結果",
          "  source/original.pdf|htm  取得した原資料（そのまま）",
          "  source/acquisition.json  取得元・取得日時・HTTP・Content-Type・形式の判定根拠・SHA-256",
          "  source/reading_*         読解用データ（ページごとの本文。必要ならOCR）",
          "  agents/copilot/          Copilot への依頼文・回答・一次読解・記録",
          "  agents/claude/           Claude への依頼文・原文照合（reviewed.json）・FpML 案・記録",
          "  result/conditions.csv    条件（UTF-8、1項目1行。leg_id と seq で反復を区別）",
          "  result/conditions.json   同じ内容の JSON",
          "  result/fpml_DRAFT.xml    FpML（検証済みなら fpml_final.xml）",
          "  result/c1_validation.xlsx 確認用の Excel（実行記録・検証結果・条件）",
          "  result/review_items.md   要確認事項")

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
  "この文書の資料・値はすべて架空のデモです。入力はローカルのデモ PDF（",
  paste0("`", mock_url, "`）で、Copilot と Claude はモック（費用なし）で動かしました。URL からの取得は HTTP 応答のモックで確かめています（10 章）。"),
  "画面の記録と表は、そのときの実際の出力です。",
  ":::",
  "",
  "## 1. 何をするか",
  "",
  "TRS（トータル・リターン・スワップ）の確認書・ターム条件を読み、参照資産・資金調達の参照金利・フィキシング・",
  "カレンダーなどの条件を、根拠（ページ・原文の引用）つきで条件表（CSV・JSON・Excel）と FpML にします。価格計算はしません。",
  "推測で条件を埋めず、空欄・選択肢・記載なしは、そのまま区別して記録します。",
  "",
  md_table(tibble::tribble(
    ~担当, ~処理,
    "PowerShell（R/c1_trs_terms/PS/）", "資料の取得と記録、読解用データ（本文・OCR）、Microsoft への委任認証、Copilot の一次読解、Claude の原文照合・FpML 作成、CSV・JSON・FpML の保存",
    "R", "入口（URL の受付）、終了コード・成果物の受取、条件・根拠・版・FpML（XML構文／公式XSD／CSV との一致）の検証、表・Excel の表示"
  )),
  "",
  "## 2. 処理の流れ",
  "",
  "1. **入力**：RStudio で `source(\"scripts/run_c1_document.R\", encoding = \"UTF-8\")` を実行し、「配置済み資料の共有URLを貼ってください:」に URL かファイルのパスを貼る",
  "2. **資料の取得**：原資料をそのまま保存し、取得元・取得日時・HTTP・Content-Type・SHA-256 を記録。取得できなければ **取得失敗** で止まる（アクセス制限は回避しない）",
  "3. **読解用データ**：PDF はページごとの本文、文字のないページは Windows 標準の OCR、HTML は本文（全体を1ページ）",
  "4. **Copilot の一次読解**：SharePoint／OneDrive はファイル参照、それ以外は本文を additionalContext で渡す（Microsoft 365 Copilot Chat API。beta／プレビュー）",
  "5. **Claude**：Copilot が成功すれば「原資料＋Copilot の結果」を原文で確かめる（**Copilot一次読解＋Claude確認**）。Copilot が未接続・利用上限・エラー・読解失敗・形式不正なら、理由を記録して原資料を単独で読む（**Claude単独処理**）",
  "6. **保存**：条件を CSV（1項目1行）・JSON に、FpML・要確認事項・根拠・実行ログとともに保存",
  "7. **R の検証**：条件・根拠・版・FpML を検証。すべて合格し、必須項目がそろい、公式 XSD にも適合したときだけ **検証済み**（FpML は最終版）。それ以外は **要確認**（FpML はドラフト）",
  "",
  "## 3. 入力の種類と、AI への渡し方",
  "",
  md_table(tibble::tribble(
    ~入力, ~"形式の判定", ~"Copilot への渡し方", ~"Claude への渡し方",
    "公開 PDF の URL", "中身・Content-Type・拡張子", "本文を additionalContext（1ページ1件）", "取得した PDF",
    "公開 HTML の URL", "同上（文字コードも判定）", "本文を additionalContext", "取得した HTML のまま",
    "ローカルの PDF・HTML", "同上", "本文を additionalContext", "同じファイル",
    "SharePoint／OneDrive", "同上", "ファイル参照（contextualResources.files）", "Graph で取得した同じ版（SHA-256・eTag で確認）"
  )),
  "",
  "## 4. 状態と終了コード",
  "",
  md_table(tibble::tribble(
    ~状態, ~"終了コード", ~意味,
    "検証済み", "0", "R の検証に合格し、必須項目がそろい、公式 XSD に適合。FpML は fpml_final.xml",
    "要確認", "0", "成果物はあるが、不合格・未確定・Claude単独処理・XSD 未検証のどれかがある。FpML は fpml_DRAFT.xml",
    "未接続", "2", "SharePoint の資料でサインインできない（資料を取得できない）",
    "取得失敗", "3", "原資料を取得できない（URL がない、アクセス拒否など）",
    "Claude 読解失敗／回答形式不正", "6／7", "Claude が原資料を読めない、または結果の形式が不正",
    "読解用データ作成失敗", "8", "本文を抜き出せず、OCR でも読めない"
  )),
  "",
  "Copilot の失敗（未接続・利用上限・API エラー・読解失敗・回答形式不正）では止まらず、Claude単独処理に進みます。",
  "",
  "## 5. 実行例 A：Copilot一次読解＋Claude確認",
  "",
  paste0("入力：`", mock_url, "`（Copilot・Claude ともモック）"),
  "",
  "```", log_lines(cases$A), "```",
  "",
  "**主な条件**（Copilot のモックには、スプレッドを 30 と読む誤りと、ルックバックの見落としを仕込んである。Claude 役が原文で修正した）",
  "",
  terms_table(cases$A),
  "",
  "**R の検証**",
  "",
  checks_table(cases$A),
  "",
  "## 6. 実行例 B：Claude単独処理（Copilot 未実施）",
  "",
  "同じ URL を Copilot の api モードで実行した。接続情報（テナントID・クライアントID）がないため Copilot は未実施となり、",
  "理由を記録して Claude が原資料を単独で読んだ（Claude はモック）。",
  "",
  "```", log_lines(cases$B), "```",
  "",
  "## 7. 実行例 C：取得失敗",
  "",
  "存在しないファイルを指定した。原資料を取得できないため、AI には進まずに止まる（URL の 404・403 も同じ扱い。10 章）。",
  "",
  "```", log_lines(cases$C), "```",
  "",
  "## 8. 成果物",
  "",
  "```", tree, "```",
  "",
  "**conditions.csv の先頭**（1項目1行）",
  "",
  "```", csv_head, "```",
  "",
  "**FpML の先頭**（ドラフト。未確認・未出力の項目は XML コメントに列挙する）",
  "",
  "```xml", fpml_head, "```",
  "",
  "## 9. いまの制約",
  "",
  "- **Copilot**：Microsoft 365 Copilot Chat API は beta（プレビュー）。テナントID・クライアントID（アプリ登録、委任権限7つと管理者同意）と Copilot ライセンスが必要。接続確認は `pwsh -File R/c1_trs_terms/PS/test_connection.ps1`",
  "- **FpML の公式 XSD**：未設定（`CURIE_FPML_XSD`）。設定するまで「検証済み」にならない",
  "- **Aladdin 用 JSON**：仕様の提示前のため作らない",
  "- **公開元の制限**：SEC などは名前と連絡先を含む User-Agent が必要（`CURIE_HTTP_USER_AGENT`）。アクセス制限は回避しない",
  "- これは独立した二重読解ではない（Copilot の一次読解を Claude が原文で確かめる）",
  "",
  "## 10. URL からの取得（HTTP 応答のモック）",
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
purrr::iwalk(cases, \(x, k) cat(k, "：", x$r$state, "（", x$r$reading %||% "", "）\n", sep = ""))
