# コンソール表示の共通関数。
# ログ収集システムで Shift-JIS と UTF-8 が混ざると文字化けするため、コンソールに出す文言は英語にする
# （進捗・完了・警告・エラー・入力プロンプト・表の見出し）。契約の固有名や実際のファイル名・パスはそのまま出す。
# 詳細（原文の引用など）はファイルに残し、通常のコンソールには出さない。
# 出力先は文言の後で改行し、ファイルなら file: <path>、フォルダなら dir: <path>（区切りは /）と表示する。

# パスの表示（区切りを / にそろえる。存在すれば絶対パスにする）
path_display_Curie <- function(path) {
  gsub("\\\\", "/", normalizePath(path, winslash = "/", mustWork = FALSE))
}

# 出力先の1行（フォルダなら dir:、それ以外は file:）
path_line_Curie <- function(path, kind = NULL) {
  kind <- kind %||% if (dir.exists(path)) "dir" else "file"
  paste0(kind, ": ", path_display_Curie(path))
}

# 文言を1行出す
say_Curie <- function(...) cat(..., "\n", sep = "")

# 文言の後に出力先を出す（例：Payload draft saved. NOT SENT. ／ file: C:/.../payload.json）
say_path_Curie <- function(message, path, kind = NULL) {
  cat(message, "\n", path_line_Curie(path, kind), "\n", sep = "")
}

# テスト結果：コンソールには英語のテスト名と PASS／FAIL だけを出し、詳細（状態・理由など）は TSV に保存する。
# results は test（英語のテスト名）と pass（TRUE／FALSE）の列と、詳細の列を持つ表。すべて合格なら TRUE を返す
report_test_results_Curie <- function(results, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  out <- results |>
    dplyr::mutate(result = dplyr::if_else(.data$pass %in% TRUE, "PASS", "FAIL"), .before = 1) |>
    dplyr::select(-"pass")
  readr::write_tsv(out, path, na = "")
  purrr::walk2(out$result, out$test, \(r, t) say_Curie(r, "  ", t))
  say_Curie(sum(out$result == "PASS"), "/", nrow(out), " passed")
  say_path_Curie("Test results saved.", path, "file")
  invisible(all(out$result == "PASS"))
}
