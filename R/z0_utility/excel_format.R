# CURIEが作るExcelの共通書式（フォント）。
#   英字・数字（ラテン文字・記号）：Cambria
#   日本語（ひらがな・カタカナ・漢字・全角記号）：MS UI Gothic
#
# 書式付きExcelのパッケージ（openxlsx2）は、DLLがWindowsのアプリケーション制御に
# 阻止されて使えないため、次の方法をとる（追加のDLLを使わない）。
#   1. writexl で書く（値・列構成はこれまでと同じ）
#   2. xlsx（ZIP）を展開し、xml2 で styles.xml と sharedStrings.xml を書き換える
#      - すべてのフォント定義を Cambria にする（数値・日付・英数字だけの文字列のセル）
#      - 日本語を含む文字列は、文字種ごとの区間（リッチテキスト）に分け、
#        英数字の区間は Cambria、日本語の区間は MS UI Gothic にする（同じセル内の混在にも対応）
#   3. zip で固め直す
# 見出し行の太字は使わない（太字のセルとリッチテキストが混在すると書式がずれるため）。

.excel_fonts_CURIE <- list(latin = "Cambria", japanese = "MS UI Gothic", size = 11)

# 日本語として扱う文字（CJK記号・かな・漢字・全角形）
.japanese_char_pattern_CURIE <- "[　-ヿ㐀-䶿一-鿿豈-﫿＀-￯]"

# 文字列を、英数字の区間と日本語の区間に分ける。
# 空白（スペース・タブ・改行）は前の区間に含め、先頭の空白は次の区間に含める
# （空白だけの区間は readxl が読み飛ばし、空白が消えるため）
split_by_script_CURIE <- function(x) {
  chars <- strsplit(x, "")[[1]]
  if (!length(chars)) return(tibble::tibble(text = character(), japanese = logical()))
  jp <- dplyr::if_else(grepl("^\\s$", chars), NA, grepl(.japanese_char_pattern_CURIE, chars))
  if (all(is.na(jp))) jp[] <- FALSE
  jp <- tidyr::fill(tibble::tibble(jp = jp), "jp", .direction = "downup")$jp
  run <- cumsum(c(TRUE, jp[-1] != jp[-length(jp)]))
  tibble::tibble(char = chars, japanese = jp, run = run) |>
    dplyr::summarise(text = paste(.data$char, collapse = ""), japanese = dplyr::first(.data$japanese), .by = "run") |>
    dplyr::select("text", "japanese")
}

# styles.xml：すべてのフォントを Cambria に（テーマのフォント指定 scheme は外す）
.set_style_fonts_CURIE <- function(path, latin) {
  doc <- xml2::read_xml(path)
  ns <- xml2::xml_ns(doc)
  fonts <- xml2::xml_find_all(doc, "//d1:fonts/d1:font", ns)
  purrr::walk(fonts, function(f) {
    xml2::xml_remove(xml2::xml_find_all(f, "d1:scheme", ns))
    name <- xml2::xml_find_first(f, "d1:name", ns)
    if (inherits(name, "xml_missing")) name <- xml2::xml_add_child(f, "name")
    xml2::xml_set_attr(name, "val", latin)
    family <- xml2::xml_find_first(f, "d1:family", ns)
    if (!inherits(family, "xml_missing")) xml2::xml_set_attr(family, "val", "1")
  })
  xml2::write_xml(doc, path, options = "as_xml")   # 字下げの空白を入れない
}

# sharedStrings.xml：日本語を含む文字列を、文字種ごとのフォントの区間に分ける
.set_string_fonts_CURIE <- function(path, fonts) {
  doc <- xml2::read_xml(path)
  ns <- xml2::xml_ns(doc)
  items <- xml2::xml_find_all(doc, "//d1:si", ns)
  purrr::walk(items, function(si) {
    text <- paste(xml2::xml_text(xml2::xml_find_all(si, ".//d1:t", ns)), collapse = "")
    if (!grepl(.japanese_char_pattern_CURIE, text)) return(invisible(NULL))
    xml2::xml_remove(xml2::xml_children(si))
    runs <- split_by_script_CURIE(text)
    purrr::pwalk(runs, function(text, japanese) {
      r <- xml2::xml_add_child(si, "r")
      pr <- xml2::xml_add_child(r, "rPr")
      xml2::xml_add_child(pr, "sz", val = as.character(fonts$size))
      xml2::xml_add_child(pr, "rFont", val = if (japanese) fonts$japanese else fonts$latin)
      xml2::xml_add_child(pr, "family", val = if (japanese) "3" else "1")
      if (japanese) xml2::xml_add_child(pr, "charset", val = "128")
      xml2::xml_add_child(r, "t", text, "xml:space" = "preserve")
    })
  })
  xml2::write_xml(doc, path, options = "as_xml")   # 字下げの空白を入れない
}

# 既存のxlsxにCURIEのフォントを適用する（上書き）
apply_excel_fonts_CURIE <- function(path, fonts = .excel_fonts_CURIE) {
  work <- tempfile("xlsx_")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  utils::unzip(path, exdir = work)
  styles <- file.path(work, "xl", "styles.xml")
  strings <- file.path(work, "xl", "sharedStrings.xml")
  if (file.exists(styles)) .set_style_fonts_CURIE(styles, fonts$latin)
  if (file.exists(strings)) .set_string_fonts_CURIE(strings, fonts)
  files <- list.files(work, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  out <- tempfile(fileext = ".xlsx")
  zip::zip(out, files = files, root = work, mode = "mirror")
  file.copy(out, path, overwrite = TRUE)
  invisible(path)
}

# CURIEのExcel出力の共通関数：writexl で書いて、フォントを適用する
write_xlsx_CURIE <- function(sheets, path, col_names = TRUE, fonts = .excel_fonts_CURIE) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writexl::write_xlsx(sheets, path, col_names = col_names, format_headers = FALSE)
  apply_excel_fonts_CURIE(path, fonts)
}

# 確認用：xlsxの中で使われているフォントを数える（文字列の区間とセルの書式）
excel_font_summary_CURIE <- function(path) {
  work <- tempfile("xlsx_")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  utils::unzip(path, exdir = work)
  read_vals <- function(file, xpath) {
    p <- file.path(work, "xl", file)
    if (!file.exists(p)) return(character())
    doc <- xml2::read_xml(p)
    xml2::xml_attr(xml2::xml_find_all(doc, xpath, xml2::xml_ns(doc)), "val")
  }
  dplyr::bind_rows(
    tibble::tibble(part = "セルの書式（styles.xml）", font = read_vals("styles.xml", "//d1:fonts/d1:font/d1:name")),
    tibble::tibble(part = "文字列の区間（sharedStrings.xml）", font = read_vals("sharedStrings.xml", "//d1:si/d1:r/d1:rPr/d1:rFont"))
  ) |> dplyr::count(.data$part, .data$font, name = "数")
}
