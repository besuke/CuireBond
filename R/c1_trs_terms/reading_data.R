# c1：原資料の本文（ページごと）。引用の照合と、ページ数・読み方（本文抽出／OCR）の記録に使う。
#   PDF ：pdftools でページごとに抜き出す。文字がほとんどないページは Windows 標準の OCR で読む（誤読がありうるので記録する）
#   HTML：文字コードを決めて本文にする（全体を1ページとする）
# Claude には原資料そのものを渡す（この本文は渡さない）。

.ocr_script_c1 <- file.path("R", "c1_trs_terms", "windows", "ocr_windows.ps1")

# HTML の文字コード：HTML の宣言（meta charset）→ UTF-8 として正しいか → Windows-1252（古い英文 HTML に多い）の順に決める
html_encoding_c1 <- function(path) {
  bytes <- readBin(path, "raw", file.size(path))
  head <- bytes[seq_len(min(4096L, length(bytes)))]
  head <- iconv(rawToChar(head[head != as.raw(0)]), "latin1", "UTF-8")   # 宣言は ASCII なので latin1 として読めば足りる
  declared <- regmatches(head, regexpr("(?i)charset\\s*=\\s*[\"']?\\K[A-Za-z0-9_-]+", head, perl = TRUE))
  if (length(declared) && !is.na(iconv("a", declared, "UTF-8"))) return(list(encoding = declared, basis = "HTML の宣言（meta charset）"))
  if (validUTF8(rawToChar(bytes[bytes != as.raw(0)]))) return(list(encoding = "UTF-8", basis = "UTF-8 として正しい"))
  list(encoding = "windows-1252", basis = "宣言がなく UTF-8 でもない（Windows-1252 とみなす）")
}

# HTML の本文（script・style を除き、段落・行・セルの境目を空白にする）
html_text_c1 <- function(path, encoding = html_encoding_c1(path)$encoding) {
  bytes <- readBin(path, "raw", file.size(path))
  text <- iconv(rawToChar(bytes[bytes != as.raw(0)]), from = encoding, to = "UTF-8", sub = "?")
  # 古い HTML の &#128;〜&#159;（例：&#147; &#148;）は、ブラウザと同じく Windows-1252 の文字（“ ” など）として読む
  text <- stringr::str_replace_all(text, "&#(1[2-5][0-9]);", \(m) {
    n <- as.integer(sub("&#(\\d+);", "\\1", m))
    ifelse(n >= 128 & n <= 159, iconv(purrr::map_chr(n, \(k) rawToChar(as.raw(k))), "windows-1252", "UTF-8", sub = ""), m)
  })
  doc <- xml2::read_html(text, encoding = "UTF-8", options = c("RECOVER", "NOERROR", "NOWARNING", "NONET"))
  xml2::xml_remove(xml2::xml_find_all(doc, "//script|//style|//noscript"))
  paste(xml2::xml_text(xml2::xml_find_all(doc, "//body//text()")), collapse = " ")
}

# Windows 標準の OCR（Windows PowerShell 5.1 で ocr_windows.ps1 を実行）。1ページ分の画像を読む
.ocr_page_c1 <- function(pdf, page, work_dir, timeout_sec = 180) {
  png <- pdftools::pdf_convert(pdf, format = "png", pages = page, dpi = 300, verbose = FALSE,
                               filenames = file.path(work_dir, sprintf("ocr_p%d.png", page)))
  out <- file.path(work_dir, sprintf("ocr_p%d.txt", page))
  res <- tryCatch(processx::run("powershell.exe", c("-NoProfile", "-ExecutionPolicy", "Bypass", "-File",
                                                    normalizePath(.ocr_script_c1), "-ImagePath", normalizePath(png), "-OutPath", out),
                                timeout = timeout_sec, error_on_status = FALSE),
                  error = function(e) list(status = -1L, timeout = FALSE))
  if (identical(res$status, 0L) && file.exists(out)) paste(readLines(out, encoding = "UTF-8", warn = FALSE), collapse = "\n") else NA_character_
}

# ページごとの本文と読み方。ocr = TRUE なら文字のないページを OCR で読む
document_pages_c1 <- function(path, kind, work_dir = tempfile("c1_reading_"), ocr = TRUE, min_chars = 20L) {
  if (identical(kind, "html")) {
    text <- html_text_c1(path)
    return(tibble::tibble(page = 1L, method = "html_text", text = text, chars = nchar(text)))
  }
  pages <- pdftools::pdf_text(path)
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
  purrr::imap(pages, function(text, i) {
    if (nchar(gsub("\\s", "", text)) >= min_chars || !ocr) return(tibble::tibble(page = i, method = "pdf_text", text = text))
    o <- .ocr_page_c1(path, i, work_dir)
    tibble::tibble(page = i, method = if (is.na(o)) "ocr_failed" else "ocr_windows", text = dplyr::coalesce(o, ""))
  }) |>
    purrr::list_rbind() |>
    dplyr::mutate(chars = nchar(.data$text))
}
