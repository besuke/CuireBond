# c1 の動き方の説明PDF（docs/c1_walkthrough.pdf）を作る。
# 架空のデモ契約書（ローカルの PDF・HTML）で実際に3つの経路を実行し、その画面出力と成果物から説明文書を作る。
# Claude はモック（費用なし）。URL からの取得は、R の取得処理に HTTP 応答のモックを渡して確かめる（9 章。ネットワークにつながない）。
#   A. 成功（PDF）
#   B. 成功（HTML。全体を1ページとして扱う）
#   C. 取得失敗（資料がない）
# Curieプロジェクトの最上位フォルダで実行する：Rscript scripts/make_c1_walkthrough.R

if (!file.exists("Curie.Rproj")) stop("Run this script from the top folder of the Curie project.")
source("R/load_Curie.R", encoding = "UTF-8")
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
  # 案件フォルダ：ログ「[c1] Finished」の次の行（dir: <案件フォルダ>）から求める
  dir <- sub("^dir:", "", out[grep("\\[c1\\] Finished", out)[1] + 1L])
  if (is.na(dir)) stop("The run stopped:\n", paste(tail(out, 15), collapse = "\n"))
  list(console = out, dir = dir, results = read_c1_case_results(dir),
       r = jsonlite::fromJSON(file.path(dir, "r_validation.json")))
}
cases <- list(A = run_case(demo$pdf, claude_demo), B = run_case(html, claude_html), C = run_case(missing_file, claude_demo))
# URL からの取得は HTTP 応答のモックで確かめる（ネットワークにつながない）
http_test <- purrr::map_chr(c("https://mock.example/ok.pdf?utm_source=x", "https://mock.example/denied.htm", "https://mock.example/missing.pdf"), function(u) {
  fetcher <- function(url, path, user_agent) {
    key <- basename(sub("[?#].*$", "", url))
    r <- switch(key, "ok.pdf" = list(200L, "application/pdf", readBin(demo$pdf, "raw", file.size(demo$pdf))),
                "denied.htm" = list(403L, "text/html", charToRaw("Forbidden")), list(404L, "text/html", charToRaw("Not Found")))
    writeBin(r[[3]], path); list(status = r[[1]], content_type = r[[2]], final_url = url)
  }
  a <- tryCatch(acquire_document_c1(u, tempfile("c1_http_"), fetcher = fetcher), error = function(e) conditionMessage(e))
  if (is.character(a)) paste0(u, " → ", a) else paste0(u, " → 取得元 ", a$source, "、HTTP ", a$http_status, "、", a$format$kind, "、SHA-256 ", substr(a$sha256, 1, 12), "…")
})

# 2. 説明文書（Quarto → Typst → PDF）
esc <- function(x) gsub("|", "\\|", x, fixed = TRUE)
md_table <- function(df) {
  df <- dplyr::mutate(df, dplyr::across(dplyr::everything(), \(x) esc(dplyr::coalesce(as.character(x), ""))))
  c(paste0("| ", paste(names(df), collapse = " | "), " |"), paste0("|", strrep("---|", ncol(df))),
    purrr::pmap_chr(df, \(...) paste0("| ", paste(c(...), collapse = " | "), " |")))
}
# 画面出力のうち、処理の段階を示す行（[c1] のログ）と結果の行
log_lines <- function(case) {
  x <- grep("\\[c1\\]|^(file|dir):|^Payload draft saved|^Processing stopped|^(CP|Start|End|Pay leg|Receive leg):", case$console, value = TRUE)
  # 手元のフォルダの場所は出さない（Windows は大文字小文字を区別しないので、区別せずに除く）
  x <- gsub("C:\\\\AnalyticFin\\\\Projects\\\\Curie\\\\|C:/AnalyticFin/Projects/Curie/", "", x, ignore.case = TRUE)
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
          "  source/reading_*         読解用データ（ページごとの本文。文字のないページは「本文なし」と記録）",
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
  "#align(center)[#text(size: 17pt, weight: \"bold\")[Curie c1：TRS 確認書の読解 ― 動き方]]",
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
    "R（R/c1_trs_terms/）", "資料の取得と記録、読解用データ（本文。文字のないページは「本文なし」と記録）、Claude Code の起動（processx・タイムアウト）、JSON・FpML の保存、検証、表示",
    "Claude Code（claude -p）", "作業フォルダの原資料（PDF・HTML）と指示書を読み、条件（reviewed.json）・FpML・要確認事項を作る（元の URL から再取得しない）"
  )),
  "",
  "## 2. 処理の流れ",
  "",
  "1. **入力**：RStudio で `source(\"scripts/run_c1_document.R\", encoding = \"UTF-8\")` を実行し、「資料のURL（またはファイルのパス）を貼ってください:」に貼る。Box の資料は Box Drive の同期フォルダのパスを貼る",
  "2. **資料の取得**：原資料をそのまま保存し、取得元・取得日時・HTTP・Content-Type・SHA-256 を記録。形式は中身・Content-Type・拡張子で判定。取得できなければ **取得失敗** で止まる（アクセス制限は回避しない）",
  "3. **読解用データ**：PDF はページごとの本文、HTML は本文（全体を1ページ）。R の引用照合に使う。文字のないページは「本文なし」と記録する（Claude は原資料を画像として読むが、そのページの引用は照合できない）",
  "4. **Claude**：原資料そのもの（PDF・HTML）を読み、全項目の値・根拠・未確定事項と FpML を作る",
  "5. **保存**：`result/conditions.json`（条件・資料の記録・要確認事項）と FpML",
  "6. **R の検証**：すべて合格し、必須項目がそろい、公式 XSD にも適合したときだけ **検証済み**（`fpml_final.xml`）。それ以外は **要確認**（`fpml_DRAFT.xml`）",
  "",
  "## 3. 状態",
  "",
  md_table(tibble::tribble(
    ~状態, ~意味,
    "検証済み", "XML構文・公式XSD・原文根拠・条件との整合がすべて合格し、必須項目がそろった。FpML は fpml_final.xml",
    "要確認", "成果物はあるが、不合格・要確認・未検証・未確定・XSD 未検証のどれかがある。FpML は fpml_DRAFT.xml",
    "デモ", "Claude がモック。検証に合格しても検証済みにしない",
    "取得失敗", "原資料を取得できない（URL がない、アクセス拒否、ファイルがないなど）。Claude には進まない",
    "読解用データ作成失敗", "原資料から本文を抜き出す処理が失敗した",
    "読解失敗", "Claude の失敗（終了コード）・タイムアウト・原資料を読めなかった。途中の結果は使わない",
    "回答形式不正", "Claude の結果の形式が不正（reviewed.json の項目不足、fpml.xml がないなど）"
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
  "- **FpML の公式 XSD**：未設定（`Curie_FPML_XSD`）。設定するまで「検証済み」にならない",
  "- **Aladdin**：送信用 JSON は下書きだけ作る（`aladdin_request_DRAFT.json`。未確定の項目は `aladdin_unresolved.json`）。送信先・認証が未提示のため送信しない",
  "- **公開元の制限**：SEC などは名前と連絡先を含む User-Agent が必要（`Curie_HTTP_USER_AGENT`）。アクセス制限は回避しない",
  "- **PDF を Claude が読む**には poppler（pdftoppm）が必要",
  "",
  "## 9. URL からの取得（HTTP 応答のモック）",
  "",
  "R の取得処理（`acquire_document_c1()`）に HTTP 応答のモックを渡した結果（ネットワークにつながない）。全体のテストは `Rscript scripts/test_c1_case_mock.R`。",
  "",
  "```", http_test, "```"
)
qmd_path <- file.path("docs", "c1_walkthrough.qmd")
writeLines(qmd, qmd_path, useBytes = TRUE)
status <- system2("quarto", c("render", qmd_path, "--to", "typst"), stdout = TRUE, stderr = TRUE)
pdf <- file.path("docs", "c1_walkthrough.pdf")
if (!file.exists(pdf)) stop("Cannot create the PDF:\n", paste(status, collapse = "\n"), call. = FALSE)
say_path_Curie(paste0("Walkthrough PDF created (", pdftools::pdf_info(pdf)$pages, " pages)."), pdf, "file")
# 案件の状態（r_validation.json に残す値）は英語で表示する
state_en <- function(s) dplyr::case_when(startsWith(s, "要確認") ~ "needs review", startsWith(s, "検証済み") ~ "verified",
                                          startsWith(s, "デモ") ~ "demo", .default = .failure_kind_en_c1(s))
purrr::iwalk(cases, \(x, k) say_Curie("Case ", k, ": ", state_en(x$r$state), " (details in r_validation.json)"))
