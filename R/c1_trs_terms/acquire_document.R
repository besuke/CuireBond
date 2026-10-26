# c1：資料の取得と記録（R で行う）。
#
# 入力：公開 PDF・HTML の URL、ローカルの PDF・HTML（Box Drive の同期フォルダのファイルも可）
# 記録：取得元、取得日時、HTTP の状態・Content-Type・最終 URL、形式（中身・Content-Type・拡張子と判定根拠）、
#       文字コード（HTML）、サイズ、SHA-256
# URL の取得には CURIE_HTTP_USER_AGENT（tmp/.Renviron 等）を User-Agent として付ける。
# 403 などのアクセス拒否は理由を表示して止める（回避しない）。

.is_url_c1 <- function(x) grepl("^https?://", x, ignore.case = TRUE)

# URL から追跡用のパラメータ（utm_ で始まるもの）を除く。ほかのパラメータは残す
strip_tracking_params_c1 <- function(url) {
  if (!.is_url_c1(url) || !grepl("?", url, fixed = TRUE)) return(url)
  fragment <- if (grepl("#", url, fixed = TRUE)) sub("^[^#]*", "", url) else ""
  base <- sub("[?].*$", "", url)
  query <- sub("#.*$", "", sub("^[^?]*[?]", "", url))
  params <- strsplit(query, "&", fixed = TRUE)[[1]]
  params <- params[nzchar(params) & !grepl("^utm_", params, ignore.case = TRUE)]
  paste0(base, if (length(params)) paste0("?", paste(params, collapse = "&")) else "", fragment)
}

# 取得の失敗（取得失敗として扱う）
.acquire_fail_c1 <- function(message) {
  stop(structure(class = c("c1_acquire_error", "error", "condition"),
                 list(message = paste0("資料を取得できません（取得失敗）: ", message), call = NULL)))
}

# URL の取得（curl）。状態コードは呼び出し側で判定する
.curl_fetch_c1 <- function(url, path, user_agent) {
  handle <- curl::new_handle(useragent = user_agent, followlocation = TRUE, maxredirs = 5L, timeout = 300L)
  res <- curl::curl_fetch_disk(url, path, handle = handle)
  list(status = res$status_code, content_type = res$type, final_url = res$url)
}

# 形式の判定：中身（先頭のバイト）を優先し、Content-Type と拡張子も記録する
.detect_format_c1 <- function(path, content_type = NA_character_, name = "") {
  bytes <- readBin(path, "raw", min(file.size(path), 4096L))
  by_content <- if (length(bytes) >= 5L && identical(rawToChar(bytes[1:5]), "%PDF-")) "pdf" else {
    # 文字コードが分からないので、バイトを latin1 として読む（見るのは ASCII の部分だけ）
    head <- tolower(iconv(rawToChar(bytes[bytes != as.raw(0)]), "latin1", "UTF-8"))
    if (grepl("<html|<!doctype html|<body|<document>", head)) "html" else NA_character_
  }
  ext <- tolower(tools::file_ext(sub("[?#].*$", "", name)))
  by_ext <- dplyr::case_when(ext == "pdf" ~ "pdf", ext %in% c("htm", "html") ~ "html", .default = NA_character_)
  ct <- tolower(dplyr::coalesce(content_type, ""))
  by_type <- dplyr::case_when(grepl("application/pdf", ct) ~ "pdf", grepl("text/html|application/xhtml", ct) ~ "html",
                              .default = NA_character_)
  kind <- dplyr::coalesce(by_content, by_type, by_ext)
  differs <- !is.na(by_content) && ((!is.na(by_type) && by_type != by_content) || (!is.na(by_ext) && by_ext != by_content))
  list(kind = kind, by_content = by_content, by_content_type = by_type, by_extension = by_ext,
       content_type = content_type, extension = ext,
       note = if (differs) "中身と Content-Type／拡張子が異なる（中身で判定）" else "")
}

# 資料を取得して案件フォルダの source/ に保存し、取得記録を返す
# fetcher：URL の取得関数（function(url, path, user_agent) → list(status, content_type, final_url)）。テストで差し替える
acquire_document_c1 <- function(source, source_dir, fetcher = .curl_fetch_c1,
                                user_agent = Sys.getenv("CURIE_HTTP_USER_AGENT", "")) {
  dir.create(source_dir, recursive = TRUE, showWarnings = FALSE)
  pasted <- trimws(gsub('^"|"$', "", source))
  src <- strip_tracking_params_c1(pasted)
  is_url <- .is_url_c1(src)
  name <- if (is_url) utils::URLdecode(basename(sub("[?#].*$", "", src))) else basename(src)
  raw <- file.path(source_dir, "original.bin")
  retrieved_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  http <- list(status = NA_integer_, content_type = NA_character_, final_url = NA_character_)
  if (is_url) {
    ua <- if (nzchar(user_agent)) user_agent else "CURIE-c1 (TRS terms reader)"
    http <- tryCatch(fetcher(src, raw, ua), error = function(e) .acquire_fail_c1(conditionMessage(e)))
    if (!identical(as.integer(http$status), 200L)) {
      unlink(raw)
      hint <- if (http$status %in% c(401L, 403L)) {
        paste0("（アクセスを拒否されました）。", if (!nzchar(user_agent))
          "公開元によっては、名前と連絡先メールを含む User-Agent を求めます（SEC など）。その場合は tmp/.Renviron に CURIE_HTTP_USER_AGENT=\"氏名 メールアドレス\" と書いてください。",
          "アクセス制限の回避はしません")
      } else ""
      .acquire_fail_c1(paste0("HTTP ", http$status, hint))
    }
  } else {
    if (!file.exists(src) || dir.exists(src)) .acquire_fail_c1(paste0("ファイルがありません: ", src))
    file.copy(src, raw, overwrite = TRUE)
  }
  format <- .detect_format_c1(raw, http$content_type, name)
  if (is.na(format$kind)) {
    unlink(raw)
    .acquire_fail_c1(paste0("PDF でも HTML でもありません（Content-Type：", dplyr::coalesce(http$content_type, "なし"),
                            "、拡張子：", format$extension, "）"))
  }
  original <- file.path(source_dir, paste0("original.", if (format$kind == "pdf") "pdf" else "htm"))
  file.rename(raw, original)
  encoding <- if (format$kind == "html") html_encoding_c1(original) else list(encoding = NA_character_, basis = NA_character_)
  list(source = src, pasted_input = pasted, source_type = if (is_url) "public_url" else "local_file", name = name,
       retrieved_at = retrieved_at, http_status = http$status, content_type = http$content_type, final_url = http$final_url,
       format = format, encoding = encoding$encoding, encoding_basis = encoding$basis,
       stored_as = file.path("source", basename(original)), path = normalizePath(original, winslash = "/"),
       bytes = file.size(original), sha256 = pdf_sha256_c1(original))
}
