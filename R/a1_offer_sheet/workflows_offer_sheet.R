# オファーシートのワークフロー（JPY／ISO）。
# 分類は建て通貨による：JPY側はJPY建て（ユーロ円債を含む）、ISO側は円以外の建て通貨。
# JPYとISOはオファーExcel・市場データの読込時点が異なるため、別々に実行できる。
# 入力ファイル・評価日・出力先・取込設定・属性取得関数はすべて引数で受け取り、
# 関数内にデモのパスや市場日は持たない。
#
# 処理の流れ（両ワークフロー共通）
#   1. オファーExcelを共通形式へ変換（convert_offer_sheet_CURIE）し、標準形式のExcelを作業フォルダへ書く
#   2. ISINで銘柄属性を取得（attribute_fetcher）し、計算終期を決め、RDSに記録する
#   3. 市場データを取込設定に従って読む（評価日以外のデータで補わない）
#   4. 移植した workflows_validate_worksheet() で結合・検証し、CURIE独自の確認を加える
#   5. 移植した Part II の関数で計算し、元のオファー情報に結果を付けてExcelへ出力する

.valuation_date_CURIE <- function(valuation_date) {
  d <- as.Date(valuation_date)
  if (length(d) != 1L || is.na(d)) stop("valuation_date には日付を1つ指定してください。", call. = FALSE)
  d
}

# 出力先が入力ファイルと同じなら、何も読まずに止める（元のExcelを上書きしない）
.check_output_path_CURIE <- function(output_path, inputs) {
  if (normalizePath(output_path, winslash = "/", mustWork = FALSE) %in%
      normalizePath(inputs, winslash = "/", mustWork = FALSE)) {
    stop("出力先が入力ファイルと同じです。元のExcelは上書きしません: ", output_path, call. = FALSE)
  }
}

.work_dir_CURIE <- function(work_dir, workflow) {
  if (is.null(work_dir)) work_dir <- file.path(tempdir(), paste0("CURIE_", workflow, "_", format(Sys.time(), "%Y%m%d%H%M%S")))
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
  normalizePath(work_dir, winslash = "/")
}

# 1〜2：オファー変換と属性取得（両ワークフロー共通）
.prepare_offers_CURIE <- function(workflow, offer_path, offer_settings, attribute_fetcher,
                                  valuation_date, work_dir) {
  offers <- do.call(convert_offer_sheet_CURIE, c(list(path = offer_path), offer_settings)) |>
    dplyr::mutate(offer_book = workflow, .before = 1)
  stamp <- format(valuation_date, "%Y%m%d")
  standard_file <- file.path(work_dir, paste0("offers_standard_", workflow, "_", stamp, ".xlsx"))
  write_offer_standard_CURIE(offers, standard_file)
  fetched <- fetch_attributes_CURIE(
    attribute_fetcher, offers$ISIN, valuation_date,
    file.path(work_dir, paste0("attributes_", workflow, "_", stamp, ".rds"))
  )
  list(offers = offers, standard_file = standard_file, attributes = fetched$attributes,
       attribute_snapshot = fetched$snapshot_path)
}

# 実行環境の記録（GaussQuantの版とソース版の照合を含む）
.environment_CURIE <- function(gaussquant_src) {
  list(environment = environment_info_CURIE(gaussquant_src),
       gaussquant_check = check_gaussquant_functions_CURIE(gaussquant_src = gaussquant_src))
}

# ---- JPY ------------------------------------------------------------------
# offer_path : 円オファーExcel
# ois_path   : JPY OIS気配のテキストファイル（複数可）
# jgb_path   : JGB時価ファイル（単価と、利回り計算に必要な銘柄属性）
# valuation_date : 評価日（オファー日・各市場データの市場日と一致させる）
# output_path    : 出力Excel
# attribute_fetcher : function(ISIN, valuation_date) -> 銘柄属性（R/z0_utility/attributes.R の列）
workflows_offer_sheet_JPY <- function(
    offer_path, ois_path, jgb_path, valuation_date, output_path, attribute_fetcher,
    offer_settings = offer_import_settings_CURIE(currency = "JPY"),
    ois_settings = text_import_settings_CURIE(),
    jgb_settings = text_import_settings_CURIE(),
    pricing_settings = pricing_settings_CURIE(),
    attribute_policy = "exact", work_dir = NULL,
    gaussquant_src = Sys.getenv("CURIE_GAUSSQUANT_SRC", file.path("..", "GaussQuant"))) {
  valuation_date <- .valuation_date_CURIE(valuation_date)
  .check_output_path_CURIE(output_path, c(offer_path, ois_path, jgb_path))
  work_dir <- .work_dir_CURIE(work_dir, "JPY")
  prepared <- .prepare_offers_CURIE("JPY", offer_path, offer_settings, attribute_fetcher, valuation_date, work_dir)
  ois <- import_ois_files_CURIE(ois_path, ois_settings, work_dir)
  jgb <- import_jgb_file_CURIE(jgb_path, jgb_settings)
  jgb_yields <- compute_jgb_yields_CURIE(jgb, pricing_settings$jgb_compound_frequency)

  worksheet <- validate_offer_worksheet_CURIE(
    "JPY", prepared$offers, prepared$standard_file, prepared$attribute_snapshot,
    ois$normalized_paths, list(jgb = jgb), valuation_date, attribute_policy = attribute_policy
  )
  pricing <- compute_offer_pricing_CURIE("JPY", worksheet, ois$quotes, list(jgb_yields = jgb_yields),
                                         valuation_date, pricing_settings)
  market_log <- dplyr::bind_rows(
    market_data_log_CURIE(ois$quotes, "OIS", valuation_date),
    market_data_log_CURIE(jgb, "JGB時価", valuation_date)
  )
  env <- .environment_CURIE(gaussquant_src)
  jgb_sheet <- jgb_yields |>
    dplyr::select(dplyr::any_of(c("marketdate", "quote_time", "ISIN", "security_name", "coupon_rate",
                                  "coupon_frequency", "maturity_date", "settlement_date", "clean_price",
                                  "price_type", "remaining_years", "compound_frequency", "compound_yield_pct",
                                  "yield_message", "jgb_price_id")))
  write_offer_results_excel_CURIE(
    "JPY", pricing, market_log, prepared$attributes,
    list("JGB利回り" = jgb_sheet, "JGB利回り_平均" = pricing$jgb_average),
    env$environment, env$gaussquant_check, output_path,
    protected_paths = c(offer_path, ois_path, jgb_path)
  )
  invisible(list(results = pricing$results, pricing = pricing, worksheet = worksheet,
                 market_log = market_log, jgb_yields = jgb_yields, attributes = prepared$attributes,
                 environment = env, output_path = output_path, work_dir = work_dir))
}

# ---- ISO ------------------------------------------------------------------
# offer_path : 外貨オファーExcel
# ois_paths  : 必要な通貨のOIS気配のテキストファイル（通貨ごとに別ファイル可）。
#              円換算ASWに使うJPYのOISも含める
# basis_path : 対JPY通貨ベーシスの市場データ
# fx_path    : FXスポットの市場データ（外貨1単位当たりの円）
workflows_offer_sheet_ISO <- function(
    offer_path, ois_paths, basis_path, fx_path, valuation_date, output_path, attribute_fetcher,
    offer_settings = offer_import_settings_CURIE(),
    ois_settings = text_import_settings_CURIE(),
    basis_settings = text_import_settings_CURIE(),
    fx_settings = text_import_settings_CURIE(),
    pricing_settings = pricing_settings_CURIE(),
    attribute_policy = "exact", work_dir = NULL,
    gaussquant_src = Sys.getenv("CURIE_GAUSSQUANT_SRC", file.path("..", "GaussQuant"))) {
  valuation_date <- .valuation_date_CURIE(valuation_date)
  .check_output_path_CURIE(output_path, c(offer_path, ois_paths, basis_path, fx_path))
  work_dir <- .work_dir_CURIE(work_dir, "ISO")
  prepared <- .prepare_offers_CURIE("ISO", offer_path, offer_settings, attribute_fetcher, valuation_date, work_dir)
  ois <- import_ois_files_CURIE(ois_paths, ois_settings, work_dir)
  basis <- import_ccs_basis_CURIE(basis_path, basis_settings)
  fx <- import_fx_spot_CURIE(fx_path, fx_settings)

  worksheet <- validate_offer_worksheet_CURIE(
    "ISO", prepared$offers, prepared$standard_file, prepared$attribute_snapshot,
    ois$normalized_paths, list(fx = fx, basis = basis), valuation_date, attribute_policy = attribute_policy
  )
  pricing <- compute_offer_pricing_CURIE("ISO", worksheet, ois$quotes, list(fx = fx, basis = basis),
                                         valuation_date, pricing_settings)
  market_log <- dplyr::bind_rows(
    market_data_log_CURIE(ois$quotes, "OIS", valuation_date),
    market_data_log_CURIE(basis, "通貨ベーシス", valuation_date),
    market_data_log_CURIE(fx, "FXスポット", valuation_date)
  )
  env <- .environment_CURIE(gaussquant_src)
  write_offer_results_excel_CURIE(
    "ISO", pricing, market_log, prepared$attributes,
    list("通貨ベーシス_平均" = pricing$basis_average, "FXスポット" = fx),
    env$environment, env$gaussquant_check, output_path,
    protected_paths = c(offer_path, ois_paths, basis_path, fx_path)
  )
  invisible(list(results = pricing$results, pricing = pricing, worksheet = worksheet,
                 market_log = market_log, attributes = prepared$attributes,
                 environment = env, output_path = output_path, work_dir = work_dir))
}
