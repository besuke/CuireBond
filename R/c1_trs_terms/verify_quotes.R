# c1：抽出JSONの根拠（ページ・引用）が、PDFに実在するかを確かめる。
# AIが記載を作り出した場合（ハルシネーション）や、ページ番号の誤りを検出するため。
# PDFの本文は pdftools で抜き出し、空白・改行・引用符の違いを吸収して照合する。

# 照合用の正規化：曲がった引用符をまっすぐに、空白・改行の連続を1つに
.normalize_text_c1 <- function(x) {
  x |>
    stringr::str_replace_all("[‘’‛]", "'") |>
    stringr::str_replace_all("[“”‟]", "\"") |>
    stringr::str_replace_all("[–—]", "-") |>
    stringr::str_squish()
}

# PDFのSHA-256（抽出時のPDFと同じものかを確かめる）
pdf_sha256_c1 <- function(path) {
  con <- file(path, open = "rb")
  on.exit(close(con))
  paste0(as.character(openssl::sha256(con)), collapse = "")   # openssl の型を外して文字列にする
}

# fields：read_trs_extractions_c1()$fields。pdf_dir：PDFを置いたフォルダ（file 列の名前で探す）
verify_trs_quotes_c1 <- function(fields, pdf_dir) {
  files <- unique(fields$file)
  pdfs <- purrr::map(files, function(f) {
    p <- file.path(pdf_dir, f)
    if (!file.exists(p)) return(NULL)
    list(text = .normalize_text_c1(pdftools::pdf_text(p)), sha256 = pdf_sha256_c1(p))
  }) |> rlang::set_names(files)
  check <- function(f, p, q) {
    if (is.na(q) || !nzchar(q)) return("引用なし")
    pdf <- pdfs[[f]]
    if (is.null(pdf)) return("PDFなし")
    target <- .normalize_text_c1(q)
    on_page <- !is.na(p) && p >= 1L && p <= length(pdf$text) && grepl(target, pdf$text[p], fixed = TRUE)
    if (on_page) return("一致")
    elsewhere <- which(purrr::map_lgl(pdf$text, \(t) grepl(target, t, fixed = TRUE)))
    if (length(elsewhere)) return(paste0("別のページにあり（p.", paste(elsewhere, collapse = ","), "）"))
    "PDFに見つからない"
  }
  quotes <- fields |>
    dplyr::mutate(quote_check = purrr::pmap_chr(list(.data$file, .data$page, .data$quote), check))
  hashes <- fields |>
    dplyr::distinct(.data$document_id, .data$file, .data$agent, .data$pdf_sha256) |>
    dplyr::mutate(
      pdf_sha256_actual = purrr::map_chr(.data$file, \(f) if (is.null(pdfs[[f]])) NA_character_ else pdfs[[f]]$sha256),
      hash_check = dplyr::case_when(
        is.na(.data$pdf_sha256_actual) ~ "PDFなし",
        is.na(.data$pdf_sha256) ~ "抽出JSONにハッシュなし",
        tolower(.data$pdf_sha256) == .data$pdf_sha256_actual ~ "一致",
        .default = "不一致（抽出時と別のPDF）"
      )
    )
  list(fields = quotes, hashes = hashes)
}
