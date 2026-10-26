# ISINのチェックディジット（ISO 6166：英字を10〜35へ置換し、Luhn法で計算）

isin_check_digit_Curie <- function(body) {
  purrr::map_int(body, function(x) {
    chars <- strsplit(toupper(x), "")[[1]]
    digits <- purrr::map_chr(chars, function(ch) {
      if (grepl("[0-9]", ch)) ch else as.character(match(ch, LETTERS) + 9L)
    }) |> paste(collapse = "") |> strsplit("") |> unlist() |> as.integer()
    # 右端（チェックディジットの直前）から1つおきに2倍する
    rev_digits <- rev(digits)
    doubled <- ifelse(seq_along(rev_digits) %% 2L == 1L, rev_digits * 2L, rev_digits)
    as.integer((10L - sum(doubled %/% 10L + doubled %% 10L) %% 10L) %% 10L)
  })
}

is_valid_isin_Curie <- function(isin) {
  isin <- toupper(trimws(isin))
  ok <- !is.na(isin) & grepl("^[A-Z]{2}[A-Z0-9]{9}[0-9]$", isin)
  ok[ok] <- as.integer(substr(isin[ok], 12, 12)) == isin_check_digit_Curie(substr(isin[ok], 1, 11))
  ok
}
