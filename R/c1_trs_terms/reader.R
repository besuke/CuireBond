# c1：読解の入口（原資料を読み、共通の形の結果 reviewed.json・fpml.xml・review_items.md を作業フォルダーに置く）。
# 後続（検証・TSV・FpML・payload・フォルダー監視・作り直し）は、どの読み方でも同じものを使う。
#
#   api          配布・運用用。R から承認済みの AI API を呼ぶ（接続先は Curie_AI_PROVIDER で選ぶ）。
#                接続先が未設定・未対応なら、英語で設定不足を表示して止まる（Claude Code へは自動で切り替えない）
#   claude_code  開発・検証用。Claude Code（claude コマンド）を呼ぶ（claude_agent.R）
#   demo         モック。用意した結果を使う（AI は呼ばない）
#   saved        保存済みの抽出結果から作り直す（AI は呼ばない）
#
# 配布用 API の接続先（アダプター）は .c1_ai_adapters に登録する。いまは試験用の mock だけ。
# 会社の承認済み API（Microsoft 365 Copilot API を検討中）は、利用許可・ライセンス・認証方式・エンドポイント・
# PDF の渡し方が確認できてから追加する（推測で作らない）。
# 認証情報は環境変数から読み、コード・Git・ログには出さない。
# 費用は、API が返す使用量と確認済みの単価（Curie_AI_PRICE_INPUT_PER_MTOK・Curie_AI_PRICE_OUTPUT_PER_MTOK、USD／100万トークン）が
# そろうときだけ計算する。それ以外は Unknown。

.reader_modes_c1 <- c("api", "claude_code", "demo", "saved")

# 配布用 API の設定（環境変数から。認証情報そのものは読まない。各アダプターが必要なときに読む）
c1_ai_api_config <- function() {
  num <- function(name) suppressWarnings(as.numeric(Sys.getenv(name, "")))
  list(provider = Sys.getenv("Curie_AI_PROVIDER", ""),
       price_input_per_mtok = num("Curie_AI_PRICE_INPUT_PER_MTOK"), price_output_per_mtok = num("Curie_AI_PRICE_OUTPUT_PER_MTOK"),
       timeout_sec = as.numeric(Sys.getenv("Curie_AI_TIMEOUT_SEC", "1800")))
}

# 試験用のアダプター（mock）：Curie_AI_MOCK_DIR の reviewed.json・fpml.xml・review_items.md を、API の答えとして返す。
# usage.json（input_tokens・output_tokens）があれば使用量として返す
.ai_adapter_mock_c1 <- function(document_path, prompt, instructions, config) {
  dir <- Sys.getenv("Curie_AI_MOCK_DIR", "")
  if (!nzchar(dir) || !dir.exists(dir)) stop("Mock AI API folder not found. Set Curie_AI_MOCK_DIR.", call. = FALSE)
  read <- function(f) if (file.exists(file.path(dir, f))) paste(readLines(file.path(dir, f), encoding = "UTF-8", warn = FALSE), collapse = "\n") else NULL
  usage <- if (file.exists(file.path(dir, "usage.json"))) jsonlite::read_json(file.path(dir, "usage.json")) else list()
  list(reviewed = read("reviewed.json"), fpml = read("fpml.xml"), review_items = read("review_items.md"), usage = usage)
}

# 登録済みのアダプター。答えは list(reviewed = JSON の文字列, fpml = XML の文字列, review_items = Markdown の文字列,
# usage = list(input_tokens, output_tokens)) で返す（読めなかったときは review_items だけ）
.c1_ai_adapters <- list(mock = .ai_adapter_mock_c1)

# 配布用 API が使えるか（使えなければ英語の理由）
.check_ai_api_c1 <- function(config = c1_ai_api_config()) {
  if (!nzchar(config$provider)) {
    return("The distribution AI API is not configured. Set Curie_AI_PROVIDER and its connection settings. Claude Code is not used automatically.")
  }
  if (is.null(.c1_ai_adapters[[config$provider]])) {
    return(paste0("AI provider '", config$provider, "' is not supported yet. Its endpoint, authentication and PDF upload method must be confirmed before it is added."))
  }
  ""
}

# 読み方が使えるか確かめ、使えなければ止める（フォルダー監視は PDF に触る前に止まる）
check_reader_c1 <- function(mode) {
  if (identical(mode, "api")) {
    problem <- .check_ai_api_c1()
    if (nzchar(problem)) stop(problem, call. = FALSE)
  }
  invisible(TRUE)
}

# 使用量と確認済みの単価から費用（USD）。どちらかが不明なら NA
.ai_cost_c1 <- function(usage, config = c1_ai_api_config()) {
  inp <- suppressWarnings(as.numeric(usage$input_tokens %||% NA)); out <- suppressWarnings(as.numeric(usage$output_tokens %||% NA))
  if (anyNA(c(inp, out, config$price_input_per_mtok, config$price_output_per_mtok))) return(NA_real_)
  (inp * config$price_input_per_mtok + out * config$price_output_per_mtok) / 1e6
}

# 費用・使用量の表示（不明なら Unknown）
.usage_text_c1 <- function(usage, cost) {
  tok <- function(x) if (is.null(x) || is.na(suppressWarnings(as.numeric(x)))) "Unknown" else format(as.numeric(x), big.mark = ",")
  paste0("tokens in/out ", tok(usage$input_tokens), " / ", tok(usage$output_tokens),
         ", cost ", if (is.na(cost)) "Unknown" else sprintf("%.4f USD", cost))
}

# 配布用 API で読む。結果は作業フォルダーに Claude Code 版と同じファイル名で置く（後続は共通）
run_ai_api_c1 <- function(agent_dir, document, config = c1_ai_api_config()) {
  unlink(file.path(agent_dir, .claude_outputs_c1))   # 古い結果を成功結果として使わない
  log <- list(mode = "api", provider = config$provider, started_at = .now_c1())
  problem <- .check_ai_api_c1(config)
  if (nzchar(problem)) return(list(state = "not_configured", log = c(log, error = problem)))
  t0 <- Sys.time()
  read <- function(f) paste(readLines(file.path(agent_dir, f), encoding = "UTF-8", warn = FALSE), collapse = "\n")
  answer <- tryCatch(.c1_ai_adapters[[config$provider]](file.path(agent_dir, document$local_name), read("prompt.md"),
                                                         read("instructions.md"), config),
                     error = function(e) e)
  log$seconds <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")))
  if (inherits(answer, "error")) return(list(state = "failed", log = c(log, error = paste0("AI API call failed: ", conditionMessage(answer)))))
  # 答えを共通のファイルにする（ないものは書かない。形式の確認は check_claude_outputs_c1 が行う）
  purrr::iwalk(c(reviewed = "reviewed.json", fpml = "fpml.xml", review_items = "review_items.md"), function(f, k) {
    if (!is.null(answer[[k]])) writeLines(enc2utf8(answer[[k]]), file.path(agent_dir, f), useBytes = TRUE)
  })
  log$usage <- answer$usage %||% list()
  log$cost_usd <- .ai_cost_c1(log$usage, config)
  list(state = "ok", log = log)
}
