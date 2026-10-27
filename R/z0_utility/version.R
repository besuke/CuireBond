# Curie のバージョン。番号はプロジェクト最上位の VERSION ファイルだけに書き、表示・記録はすべてここから読む。
# 開発環境で Git があれば、コミットの短い ID も添える（配布先で Git がなければ省く）。

curie_version_Curie <- function(root = Sys.getenv("Curie_ROOT", ".")) {
  f <- file.path(root, "VERSION")
  if (file.exists(f)) trimws(readLines(f, n = 1L, warn = FALSE)) else "unknown"
}

curie_commit_Curie <- function(root = Sys.getenv("Curie_ROOT", ".")) {
  if (!dir.exists(file.path(root, ".git")) || !nzchar(Sys.which("git"))) return("")
  out <- tryCatch(suppressWarnings(system2("git", c("-C", shQuote(root), "rev-parse", "--short", "HEAD"), stdout = TRUE, stderr = FALSE)),
                  error = function(e) character())
  if (length(out) && grepl("^[0-9a-f]{7,}$", out[1])) out[1] else ""
}

# 表示用の1行（例：Version 0.1.0 (commit 77d5c39) / R 4.6.0）
curie_version_line_Curie <- function(root = Sys.getenv("Curie_ROOT", ".")) {
  commit <- curie_commit_Curie(root)
  paste0("Version ", curie_version_Curie(root), if (nzchar(commit)) paste0(" (commit ", commit, ")"),
         " / R ", R.version$major, ".", R.version$minor)
}

# 記録用（manifest.json・validation.json に残す）
curie_version_info_Curie <- function(root = Sys.getenv("Curie_ROOT", ".")) {
  list(curie = curie_version_Curie(root), commit = curie_commit_Curie(root),
       r = paste0(R.version$major, ".", R.version$minor))
}
