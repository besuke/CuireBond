# c1：対象文書の取得（URL またはローカルファイル。PDF または HTML）。
# URLなら R がダウンロードし、ローカルファイルなら取込用フォルダへ複製する（元のファイルには触れない）。
# HTMLは Chrome（なければ Edge）のヘッドレス印刷でPDFへ変換し、以降はPDFとして扱う（ページ・引用確認・AIの読解）。
# 元のHTMLも取込用フォルダに残し、そのSHA-256を記録する（変換後のPDFは作成日時を含むため、毎回ハッシュが変わる）。
# 記録：取得元、取得方法、ファイル名、元の種類、取得日時、サイズ、SHA-256、HTTPの状態・種類（URLの場合）
#
# User-Agent：環境変数 CURIE_HTTP_USER_AGENT（例：.Renviron に書く）。SEC（sec.gov）は、
# 名前と連絡先メールを含む User-Agent でないと 403 を返す。コードには連絡先を書かない。
# ブラウザ：環境変数 CURIE_CHROME で実行ファイルを指定できる（未指定なら Chrome、Edge の標準の場所を探す）。

.is_url_c1 <- function(x) grepl("^https?://", x, ignore.case = TRUE)

# URLから追跡用のパラメータ（utm_source など utm_ で始まるもの）を取り除く。ほかのパラメータは残す
strip_tracking_params_c1 <- function(url) {
  if (!.is_url_c1(url) || !grepl("?", url, fixed = TRUE)) return(url)
  base <- sub("[?].*$", "", url)
  fragment <- if (grepl("#", url, fixed = TRUE)) sub("^[^#]*", "", url) else ""
  query <- sub("#.*$", "", sub("^[^?]*[?]", "", url))
  params <- strsplit(query, "&", fixed = TRUE)[[1]]
  params <- params[nzchar(params) & !grepl("^utm_", params, ignore.case = TRUE)]
  paste0(base, if (length(params)) paste0("?", paste(params, collapse = "&")) else "", fragment)
}

# 文書の種類を中身で判定する（拡張子は信用しない）。PDF・HTML以外は扱わない
.document_kind_c1 <- function(path) {
  con <- file(path, "rb"); head <- readBin(con, "raw", 2048L); close(con)
  if (length(head) >= 5L && identical(rawToChar(head[1:5]), "%PDF-")) return("pdf")
  text <- tolower(rawToChar(head[head != as.raw(0)]))
  if (grepl("<html|<!doctype html|<body|<document>", text)) return("html")
  NA_character_
}


# URLの末尾からファイル名を取る（?以降・拡張子を除き、.pdf を付ける）
.file_name_from_source_c1 <- function(source) {
  name <- tools::file_path_sans_ext(basename(sub("[?#].*$", "", source)))
  if (!nzchar(name)) name <- "document"
  paste0(gsub("[^A-Za-z0-9._-]", "_", name), ".pdf")
}

# HTML → PDF の変換に使うブラウザ
.find_browser_c1 <- function() {
  candidates <- c(Sys.getenv("CURIE_CHROME", ""),
                  "C:/Program Files/Google/Chrome/Application/chrome.exe",
                  "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe",
                  "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe",
                  "C:/Program Files/Microsoft/Edge/Application/msedge.exe")
  found <- purrr::keep(candidates, \(p) nzchar(p) && file.exists(p))
  if (!length(found)) stop("HTMLをPDFへ変換するブラウザ（Chrome／Edge）が見つかりません。CURIE_CHROME で指定してください。", call. = FALSE)
  found[[1]]
}

# HTMLをPDFへ変換する（ヘッドレス印刷。ヘッダー・フッターは付けない。一時プロファイルを使い、終わったら消す）
html_to_pdf_c1 <- function(html_path, pdf_path) {
  q <- function(x) paste0('"', x, '"')   # Windows のコマンドラインは二重引用符で囲む
  profile <- tempfile("curie_browser_")
  on.exit(unlink(profile, recursive = TRUE), add = TRUE)
  html_url <- paste0("file:///", normalizePath(html_path, winslash = "/"))
  status <- system2(.find_browser_c1(),
                    c("--headless=new", "--disable-gpu", "--no-pdf-header-footer",
                      paste0("--user-data-dir=", q(profile)), paste0("--print-to-pdf=", q(normalizePath(pdf_path, winslash = "/", mustWork = FALSE))), q(html_url)),
                    stdout = FALSE, stderr = FALSE)
  if (!file.exists(pdf_path)) stop("HTMLをPDFへ変換できませんでした（ブラウザの終了コード ", status, "）: ", html_path, call. = FALSE)
  invisible(pdf_path)
}

acquire_trs_document_c1 <- function(source, intake_dir,
                                    user_agent = Sys.getenv("CURIE_HTTP_USER_AGENT", "CURIE-c1 (TRS terms reader)")) {
  if (length(source) != 1L || is.na(source) || !nzchar(source)) stop("source にURLまたはファイルのパスを1つ指定してください。", call. = FALSE)
  dir.create(intake_dir, recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile()
  retrieved_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  if (.is_url_c1(source)) {
    handle <- curl::new_handle(useragent = user_agent, followlocation = TRUE)
    res <- curl::curl_fetch_disk(source, tmp, handle = handle)
    http_status <- res$status_code
    content_type <- res$type
    final_url <- res$url
    if (http_status != 200L) {
      hint <- if (http_status == 403L && !nzchar(Sys.getenv("CURIE_HTTP_USER_AGENT"))) {
        paste0("\n  公開元が自動取得を拒否しました。SEC などは名前と連絡先メールを含む User-Agent を求めます。",
               "\n  .Renviron に CURIE_HTTP_USER_AGENT=\"氏名 メールアドレス\" を書いて R を再起動してください。",
               "\n  または、ブラウザで保存したファイルのパスを貼ってください。")
      } else ""
      stop("ダウンロードに失敗しました（HTTP ", http_status, "）: ", source, hint, call. = FALSE)
    }
    method <- "download"
  } else {
    if (!file.exists(source)) stop("ファイルがありません: ", source, call. = FALSE)
    file.copy(source, tmp, overwrite = TRUE)
    http_status <- NA_integer_; content_type <- NA_character_; final_url <- NA_character_
    method <- "local_copy"
  }
  source_kind <- .document_kind_c1(tmp)
  if (is.na(source_kind)) stop("PDFでもHTMLでもありません: ", source, call. = FALSE)
  file_name <- .file_name_from_source_c1(source)
  source_sha <- pdf_sha256_c1(tmp)
  if (source_kind == "html") {
    # 元のHTMLを残してから、PDFへ変換する
    html_name <- paste0(substr(source_sha, 1, 12), "_", tools::file_path_sans_ext(file_name), ".htm")
    file.copy(tmp, file.path(intake_dir, html_name), overwrite = TRUE)
    pdf_tmp <- tempfile(fileext = ".pdf")
    html_to_pdf_c1(file.path(intake_dir, html_name), pdf_tmp)
    unlink(tmp)
    tmp <- pdf_tmp
    method <- paste0(method, "+html_to_pdf")
  }
  sha <- pdf_sha256_c1(tmp)
  # 同じ名前でも版が違えば別ファイルとして残す（SHA-256の先頭12文字を付ける）
  stored <- file.path(intake_dir, paste0(substr(sha, 1, 12), "_", file_name))
  file.copy(tmp, stored, overwrite = TRUE)
  unlink(tmp)
  record <- tibble::tibble(
    source = source, method = method, final_url = final_url, file_name = file_name,
    source_kind = source_kind, source_sha256 = source_sha,
    stored_path = normalizePath(stored, winslash = "/"), retrieved_at = retrieved_at,
    bytes = file.size(stored), sha256 = sha, http_status = http_status, content_type = content_type,
    pages = pdftools::pdf_info(stored)$pages
  )
  manifest <- file.path(intake_dir, "acquisition_manifest.csv")
  readr::write_csv(record, manifest, append = file.exists(manifest))
  record
}
