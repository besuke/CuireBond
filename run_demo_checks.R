# CURIE デモの確認：JPY・ISOを別々に実行し、入力パス・形式を変えても同じ結果になること、
# 市場日の不一致を別の日付で補わないこと、元Excelを上書きしないことなどを確かめる。
# CURIEプロジェクトの最上位フォルダで実行する。データはすべて架空のテスト用。

source("R/load_CURIE.R", encoding = "UTF-8")
source("R/demo/prepare_demo_inputs.R", encoding = "UTF-8")
suppressPackageStartupMessages(library(dplyr))

demo <- prepare_demo_inputs_CURIE()
vd <- demo$marketdate
out_dir <- file.path("output", "checks")
jpy_args <- list(offer_path = demo$offer_jpy, ois_path = demo$ois_jpy, jgb_path = demo$jgb,
                 valuation_date = vd, attribute_fetcher = demo$attribute_fetcher,
                 offer_settings = offer_import_settings_CURIE(sheet = "円債オファー", currency = "JPY"))
iso_args <- list(offer_path = demo$offer_iso, ois_paths = c(demo$ois_usd, demo$ois_eur, demo$ois_jpy),
                 basis_path = demo$basis, fx_path = demo$fx, valuation_date = vd,
                 attribute_fetcher = demo$attribute_fetcher)
run_jpy <- function(..., name) do.call(workflows_offer_sheet_JPY, modifyList(jpy_args, list(
  ..., output_path = file.path(out_dir, paste0(name, ".xlsx")), work_dir = file.path(out_dir, "work", name))))
run_iso <- function(..., name) do.call(workflows_offer_sheet_ISO, modifyList(iso_args, list(
  ..., output_path = file.path(out_dir, paste0(name, ".xlsx")), work_dir = file.path(out_dir, "work", name))))
key_cols <- c("ISIN", "native_asw_bp", "jgb_spread_bp", "jpy_asw_bp", "pricing_status")
same_results <- function(a, b) isTRUE(all.equal(select(a, any_of(key_cols)), select(b, any_of(key_cols))))

# ---- 1. JPYだけ、ISOだけで実行 -----------------------------------------------
jpy <- run_jpy(name = "JPY_base")
iso <- run_iso(name = "ISO_base")
cat("\n== 1. 単独実行 ==\n")
print(count(jpy$results, pricing_status))
print(count(iso$results, pricing_status))

# ---- 2. 入力パスと形式を変える ---------------------------------------------
# 別フォルダへコピーしたファイル、カンマ区切り・CP932・日本語列名のOISを使う
alt_dir <- file.path("data", "demo_alt")
dir.create(alt_dir, recursive = TRUE, showWarnings = FALSE)
alt <- purrr::map_chr(c(offer_jpy = demo$offer_jpy, offer_iso = demo$offer_iso, jgb = demo$jgb,
                        fx = demo$fx, basis = demo$basis), function(p) {
  to <- file.path(alt_dir, paste0("copy_", basename(p)))
  file.copy(p, to, overwrite = TRUE)
  to
})
alt_ois <- purrr::map_chr(c(JPY = "JPY", USD = "USD", EUR = "EUR"), \(ccy) write_demo_ois_alternative_CURIE(
  file.path(alt_dir, paste0("ois_", ccy, "_cp932.csv")), ccy, vd))
alt_settings <- demo_ois_alternative_settings_CURIE()
jpy_alt <- run_jpy(offer_path = alt[["offer_jpy"]], ois_path = alt_ois[["JPY"]], jgb_path = alt[["jgb"]],
                   ois_settings = alt_settings, name = "JPY_alt")
iso_alt <- run_iso(offer_path = alt[["offer_iso"]], ois_paths = unname(alt_ois[c("USD", "EUR", "JPY")]),
                   basis_path = alt[["basis"]], fx_path = alt[["fx"]], ois_settings = alt_settings, name = "ISO_alt")
cat("\n== 2. 別パス・別形式 ==\n")
cat("JPY 同じ結果:", same_results(jpy$results, jpy_alt$results),
    " / ISO 同じ結果:", same_results(iso$results, iso_alt$results), "\n")
cat("使ったOISファイル（ISO）:", paste(unique(iso_alt$market_log$source_file[iso_alt$market_log$data_type == "OIS"]), collapse = ", "), "\n")

# ---- 3. 市場日が評価日と違うデータは使わない --------------------------------
shifted_jgb <- file.path(alt_dir, "jgb_shifted.txt")
read_text_table_CURIE(demo$jgb) |> select(-"source_file") |>
  mutate(marketdate = format(vd + 1L)) |> readr::write_delim(shifted_jgb, delim = "|", na = "")
jpy_shift <- run_jpy(jgb_path = shifted_jgb, name = "JPY_jgb_shifted")
shifted_fx <- file.path(alt_dir, "fx_shifted.txt")
read_text_table_CURIE(demo$fx) |> select(-"source_file") |>
  mutate(marketdate = format(vd - 1L)) |> readr::write_delim(shifted_fx, delim = "|", na = "")
iso_shift <- run_iso(fx_path = shifted_fx, name = "ISO_fx_shifted")
cat("\n== 3. 市場日の不一致 ==\n")
print(jpy_shift$market_log |> select(data_type, source_file, marketdates, status))
cat("JPY:", unique(jpy_shift$results$validation_message[jpy_shift$results$coupon_type == "FIXED"]), "\n")
cat("ISO:", unique(iso_shift$results$validation_message[iso_shift$results$coupon_type == "FIXED"]), "\n")

# ---- 4. 元Excelを上書きしない ----------------------------------------------
overwrite <- tryCatch(do.call(workflows_offer_sheet_JPY, modifyList(jpy_args, list(
  output_path = demo$offer_jpy, work_dir = file.path(out_dir, "work", "overwrite")))), error = function(e) e)
cat("\n== 4. 上書きの防止 ==\n", conditionMessage(overwrite), "\n")

# ---- 5. 確認 ---------------------------------------------------------------
fixed_jpy <- jpy$results$coupon_type == "FIXED"
fixed_iso <- is.na(iso$results$first_call_date)
sheets <- readxl::excel_sheets(jpy$output_path)
stopifnot(
  nrow(jpy$results) == 6L, nrow(iso$results) == 6L,
  all(jpy$results$pricing_status[fixed_jpy] == "success"),
  jpy$results$pricing_status[!fixed_jpy] == "unsupported",
  all(iso$results$pricing_status[fixed_iso] == "success"),
  iso$results$pricing_status[!fixed_iso] == "unsupported",
  any(jpy$results$ISIN == "XSTESTEY0017" & jpy$results$pricing_status == "success"),   # ユーロ円債はJPY側
  all(!is.na(jpy$results$jgb_spread_bp[fixed_jpy])),
  all(!is.na(iso$results$jpy_asw_bp[fixed_iso])),
  all(abs(iso$results$fair_spread_npv_jpy[fixed_iso]) < 1e-6),
  all(abs(iso$results$ccs_repricing_npv_jpy[fixed_iso]) < 1e-6),
  same_results(jpy$results, jpy_alt$results), same_results(iso$results, iso_alt$results),
  all(jpy_shift$results$pricing_status[fixed_jpy] == "skipped"),
  all(grepl("missing_JGB_prices", jpy_shift$results$validation_message[fixed_jpy])),
  all(grepl("missing_FX_spot", iso_shift$results$validation_message[fixed_iso])),
  inherits(overwrite, "error"),
  all(c("オファー結果", "市場データ", "銘柄属性", "JGB利回り", "JGB利回り_平均") %in% sheets),
  all(c("通貨ベーシス_平均", "FXスポット") %in% readxl::excel_sheets(iso$output_path)),
  all(jpy$environment$gaussquant_check$identical)
)
cat("\n確認：すべて合格\n")
