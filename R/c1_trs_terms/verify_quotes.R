# c1：根拠（原文の引用）の照合に使う補助。照合そのものは validate_results.R で行う。
# AIが記載を作り出した場合（ハルシネーション）や、ページ番号の誤りを検出するため、
# 空白・改行・引用符の違いを吸収して照合する。

# 照合用の正規化：曲がった引用符をまっすぐに、空白・改行の連続を1つに
.normalize_text_c1 <- function(x) {
  x |>
    stringr::str_replace_all("[‘’‛]", "'") |>
    stringr::str_replace_all("[“”‟]", "\"") |>
    stringr::str_replace_all("[–—]", "-") |>
    stringr::str_squish()
}

# ファイルのSHA-256（取得時の原資料と同じものかを確かめる）
pdf_sha256_c1 <- function(path) {
  con <- file(path, open = "rb")
  on.exit(close(con))
  paste0(as.character(openssl::sha256(con)), collapse = "")   # openssl の型を外して文字列にする
}
