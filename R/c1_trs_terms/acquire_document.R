# c1：対象PDFの取得（URL またはローカルファイル）。
# URLなら R がダウンロードし、ローカルファイルなら取込用フォルダへ複製する（元のファイルには触れない）。
# どちらも同じ後続処理へ渡せるよう、取込用フォルダのファイルと取得記録を返す。
# 記録：取得元、取得方法、ファイル名、取得日時、サイズ、SHA-256、HTTPの状態・種類（URLの場合）

.is_url_c1 <- function(x) grepl("^https?://", x, ignore.case = TRUE)

# URLの末尾からファイル名を取る（?以降は除く）
.file_name_from_source_c1 <- function(source) {
  name <- basename(sub("[?#].*$", "", source))
  if (!nzchar(name) || !grepl("[.]pdf$", name, ignore.case = TRUE)) name <- paste0(name, ".pdf")
  gsub("[^A-Za-z0-9._-]", "_", name)
}

acquire_trs_document_c1 <- function(source, intake_dir, user_agent = "CURIE-c1 (TRS terms reader)") {
  if (length(source) != 1L || is.na(source) || !nzchar(source)) stop("source にURLまたはファイルのパスを1つ指定してください。", call. = FALSE)
  dir.create(intake_dir, recursive = TRUE, showWarnings = FALSE)
  file_name <- .file_name_from_source_c1(source)
  tmp <- tempfile(fileext = ".pdf")
  retrieved_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  if (.is_url_c1(source)) {
    handle <- curl::new_handle(useragent = user_agent, followlocation = TRUE)
    res <- curl::curl_fetch_disk(source, tmp, handle = handle)
    http_status <- res$status_code
    content_type <- res$type
    final_url <- res$url
    if (http_status != 200L) stop("ダウンロードに失敗しました（HTTP ", http_status, "）: ", source, call. = FALSE)
    method <- "download"
  } else {
    if (!file.exists(source)) stop("ファイルがありません: ", source, call. = FALSE)
    file.copy(source, tmp, overwrite = TRUE)
    http_status <- NA_integer_; content_type <- NA_character_; final_url <- NA_character_
    method <- "local_copy"
  }
  con <- file(tmp, "rb"); magic <- readBin(con, "raw", 5L); close(con)
  if (!identical(rawToChar(magic), "%PDF-")) stop("PDFではありません（先頭が %PDF- でない）: ", source, call. = FALSE)
  sha <- pdf_sha256_c1(tmp)
  # 同じ名前でも版が違えば別ファイルとして残す（SHA-256の先頭12文字を付ける）
  stored <- file.path(intake_dir, paste0(substr(sha, 1, 12), "_", file_name))
  file.copy(tmp, stored, overwrite = TRUE)
  unlink(tmp)
  record <- tibble::tibble(
    source = source, method = method, final_url = final_url, file_name = file_name,
    stored_path = normalizePath(stored, winslash = "/"), retrieved_at = retrieved_at,
    bytes = file.size(stored), sha256 = sha, http_status = http_status, content_type = content_type,
    pages = pdftools::pdf_info(stored)$pages
  )
  manifest <- file.path(intake_dir, "acquisition_manifest.csv")
  readr::write_csv(record, manifest, append = file.exists(manifest))
  record
}
