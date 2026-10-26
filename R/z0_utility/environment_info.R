# 実行環境の記録と、GaussQuant（インストール済み版）とソース版の照合。
# 当面はインストール済み版を使う。版番号が違っても、Curieが使う関数の中身が
# ソース版と一致していることを確認できた範囲で進める。

# Curieと移植関数（R/z0_utility/lagrange/）が呼ぶGaussQuantの関数（内部の補助関数を含む）
.curie_gaussquant_functions <- c(
  "fixed_rate_bond_GQL", "day_counter_GQL", "date_GQL", "iso_GQL", "set_eval_date_GQL",
  "safe_iso_GQH", "leg_to_cashflow_tbl_GQL", "cashflow_standardise_GQL",
  "bond_compound_yield_from_price_YEN_GQL", "bond_price_from_compound_yield_YEN_GQL",
  "bond_year_fraction_YEN_GQL", "compound_values_YEN_GQH", "newton_rate_YEN_GQH",
  "validate_frequencies_YEN_GQH", "truncate_decimal_YEN_GQH"
)

.description_field_Curie <- function(path, field) {
  if (!file.exists(path)) return(NA_character_)
  value <- read.dcf(path, fields = field)[1, 1]
  if (is.na(value)) NA_character_ else unname(value)
}

# 使用中のパッケージの版。GaussQuant はインストール元のコミットとソース版も記録する
environment_info_Curie <- function(gaussquant_src = Sys.getenv("Curie_GAUSSQUANT_SRC", file.path("..", "GaussQuant"))) {
  pkgs <- c("dplyr", "purrr", "tibble", "tidyr", "stringr", "readr", "readxl", "writexl",
            "collapse", "QuantLib", "GaussQuant")
  installed <- purrr::map(pkgs, function(p) {
    desc <- file.path(find.package(p), "DESCRIPTION")
    tibble::tibble(item = p, value = as.character(utils::packageVersion(p)),
                   note = paste0("built: ", .description_field_Curie(desc, "Built")))
  }) |> purrr::list_rbind()
  gq_desc <- file.path(find.package("GaussQuant"), "DESCRIPTION")
  src_desc <- file.path(gaussquant_src, "DESCRIPTION")
  dplyr::bind_rows(
    tibble::tibble(item = "R", value = R.version.string, note = ""),
    installed,
    tibble::tibble(
      item = c("GaussQuant インストール元", "GaussQuant ソース版"),
      value = c(.description_field_Curie(gq_desc, "RemoteSha"), .description_field_Curie(src_desc, "Version")),
      note = c(paste0("RemoteType: ", .description_field_Curie(gq_desc, "RemoteType")),
               paste0("ソース: ", normalizePath(gaussquant_src, winslash = "/", mustWork = FALSE)))
    )
  )
}

# インストール済み版とソース版の関数本体・引数を比べる
check_gaussquant_functions_Curie <- function(
    functions = .curie_gaussquant_functions,
    gaussquant_src = Sys.getenv("Curie_GAUSSQUANT_SRC", file.path("..", "GaussQuant"))) {
  src_dir <- file.path(gaussquant_src, "R")
  if (!dir.exists(src_dir)) {
    return(tibble::tibble(function_name = functions, installed = NA, source = NA, identical = NA,
                          note = paste("ソースが見つからない:", src_dir)))
  }
  src <- new.env()
  purrr::walk(list.files(src_dir, pattern = "\\.R$", full.names = TRUE),
              \(f) try(sys.source(f, envir = src, keep.source = FALSE), silent = TRUE))
  ns <- asNamespace("GaussQuant")
  purrr::map(functions, function(f) {
    a <- if (exists(f, ns, inherits = FALSE)) get(f, ns) else NULL
    b <- if (exists(f, src, inherits = FALSE)) get(f, src) else NULL
    same <- !is.null(a) && !is.null(b) &&
      identical(deparse(body(a)), deparse(body(b))) && identical(deparse(formals(a)), deparse(formals(b)))
    tibble::tibble(function_name = f, installed = !is.null(a), source = !is.null(b), identical = same, note = "")
  }) |> purrr::list_rbind()
}
