# 依存関係の確認と、移植したPart IIの関数（R/Z0_utility/lagrange/）の読込。
# このファイルは確認対象のパッケージに頼らず、R標準の関数だけで書く。
# （tibble等が読めない環境でも、何が不足しているかを表示できるようにするため）
#
# R/Z0_utility/lagrange/ は LagrangeFinance/R から移植した関数で、計算ロジックは変更していない。
# GaussQuant と QuantLib はパッケージとして利用し、関数はコピーしない。

# 移植ファイルごとの必要パッケージ（各ファイルの .ws_require() と本文から確認したもの）
.curie_lagrange_files <- list(
  "import_data.R"                  = c("readxl", "readr", "dplyr", "purrr", "tibble"),
  "workflows_validate_worksheet.R" = c("collapse", "dplyr", "purrr", "tibble", "readxl", "readr"),
  "instance_swap_curve.R"          = c("QuantLib", "dplyr", "purrr", "tibble"),
  "compute_asset_swap_ISO.R"       = c("QuantLib", "GaussQuant", "dplyr", "purrr", "tibble"),
  "compute_asset_swap_basis_JPY.R" = c("QuantLib", "dplyr", "purrr", "tibble", "stats")
)

# 読込順は依存の順（workflows_validate_worksheet.R は import_data.R の補助関数を使う）
.curie_lagrange_default <- c(
  "import_data.R", "workflows_validate_worksheet.R", "instance_swap_curve.R",
  "compute_asset_swap_ISO.R", "compute_asset_swap_basis_JPY.R"
)

# CURIE本体で使うパッケージ
.curie_packages <- c("dplyr", "purrr", "tibble", "tidyr", "stringr", "readr", "readxl",
                     "writexl", "rlang", "collapse", "QuantLib", "GaussQuant")

# 不足パッケージを一覧で示して停止する。インストールは行わない。
check_packages_CURIE <- function(packages = .curie_packages) {
  ok <- vapply(packages, function(p) {
    tryCatch({ loadNamespace(p); TRUE }, error = function(e) FALSE)
  }, logical(1))
  if (!all(ok)) {
    stop("読み込めないパッケージがあります: ", paste(packages[!ok], collapse = ", "),
         "\n未インストール、またはDLLの読込が阻止されていないか確認してください。", call. = FALSE)
  }
  invisible(TRUE)
}

# 移植した関数をグローバル環境へ読み込む（Part IIの関数名をそのまま使うため）
load_lagrange_functions_CURIE <- function(files = .curie_lagrange_default, dir = file.path("R", "Z0_utility", "lagrange")) {
  unknown <- setdiff(files, names(.curie_lagrange_files))
  if (length(unknown)) stop("依存関係が未確認のファイルです: ", paste(unknown, collapse = ", "), call. = FALSE)
  check_packages_CURIE(unique(unlist(.curie_lagrange_files[files])))
  for (f in files) sys.source(file.path(dir, f), envir = globalenv(), keep.source = FALSE)
  invisible(file.path(dir, files))
}
