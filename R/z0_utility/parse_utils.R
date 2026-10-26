# 文字・日付・数値の正規化（共通）。全角→半角、日付・数値の解釈と、失敗時の行番号付きエラー。

# ---- 文字・日付・数値の正規化 ---------------------------------------------

# 全角英数記号・全角空白を半角へ
to_halfwidth_Curie <- function(x) {
  full <- intToUtf8(c(0x3000, 0xFF01:0xFF5E), multiple = TRUE)
  half <- intToUtf8(c(0x20, 0x21:0x7E), multiple = TRUE)
  chartr(paste(full, collapse = ""), paste(half, collapse = ""), as.character(x))
}

.blank_Curie <- function(x) is.na(x) | stringr::str_trim(as.character(x)) == ""

# 失敗した値とExcel行番号を添えて停止する
.stop_parse_Curie <- function(label, values, rows) {
  stop(label, " を解釈できません: ",
       paste0(values, "（", rows, "行目）", collapse = ", "), call. = FALSE)
}

# 対応形式：YYYYMMDD、YYYY-MM-DD、YYYY/MM/DD、YYYY.MM.DD、YYYY年M月D日、Excelの日付番号
parse_date_Curie <- function(x, label = "日付", rows = seq_along(x)) {
  text <- stringr::str_squish(to_halfwidth_Curie(x))
  blank <- .blank_Curie(text)
  out <- suppressWarnings(dplyr::case_when(
    blank ~ as.Date(NA),
    stringr::str_detect(text, "^\\d{8}$") ~ as.Date(text, format = "%Y%m%d"),
    stringr::str_detect(text, "^\\d{4}[-/.]\\d{1,2}[-/.]\\d{1,2}$") ~
      as.Date(stringr::str_replace_all(text, "[/.]", "-"), format = "%Y-%m-%d"),
    stringr::str_detect(text, "^\\d{4}年\\d{1,2}月\\d{1,2}日$") ~ as.Date(text, format = "%Y年%m月%d日"),
    stringr::str_detect(text, "^\\d{1,5}(\\.\\d+)?$") ~
      as.Date(floor(as.numeric(text)), origin = "1899-12-30"),
    .default = as.Date(NA)
  ))
  bad <- !blank & is.na(out)
  if (any(bad)) .stop_parse_Curie(label, text[bad], rows[bad])
  out
}

# 桁区切りのカンマ・空白・全角を許容する。単位の換算は呼出側で行う。
parse_number_Curie <- function(x, label = "数値", rows = seq_along(x)) {
  text <- stringr::str_remove_all(to_halfwidth_Curie(x), "[,\\s]")
  blank <- .blank_Curie(text)
  ok <- blank | stringr::str_detect(text, "^[+-]?(\\d+\\.?\\d*|\\.\\d+)([eE][+-]?\\d+)?$")
  if (any(!ok)) .stop_parse_Curie(label, text[!ok], rows[!ok])
  out <- suppressWarnings(as.numeric(text))
  out[blank] <- NA_real_
  out
}

