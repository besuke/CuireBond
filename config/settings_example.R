# 実データ用の設定のサンプル（雛形）。値はすべて仮のもの。
# 使うときは config/settings_local.R にコピーして書き換える（*_local.R はGit管理対象外）。
# APIキーや認証情報はこのファイルに書かず、環境変数（.Renviron、Git管理対象外）から読む。

source("R/load_Curie.R", encoding = "UTF-8")

valuation_date <- "YYYY-MM-DD"   # 評価日（オファー日・各市場データの市場日と一致させる）

# ---- 取込設定（実ファイルの形式が決まったら書き換える） ----------------------
jpy_offer_settings <- offer_import_settings_Curie(
  sheet = NULL,          # NULL なら見出しから自動判定。候補が複数ならシート名を指定
  header_row = NULL,
  column_map = NULL,     # 例：c(offer_price = "単価", broker_names = "業者名")
  currency = "JPY"       # 通貨列がないJPYブック用
)
iso_offer_settings <- offer_import_settings_Curie()

ois_settings <- text_import_settings_Curie(
  delim = "|",           # 区切り文字
  encoding = "UTF-8",    # 文字コード（例 "CP932"）
  column_map = NULL      # 例：c(marketdate = "市場日", tenor = "期間", rate_pct = "金利(%)")
)
jgb_settings <- text_import_settings_Curie()
basis_settings <- text_import_settings_Curie()
fx_settings <- text_import_settings_Curie()

# ASWの翌日物レッグの払い方（現段階ではデモの仮定。実際の条件が決まったら変更）
pricing_settings <- pricing_settings_Curie(asw_payment_frequency = "Semiannual", asw_payment_lag = 0L)

# ---- 銘柄属性の取得関数 ------------------------------------------------------
# Aladdin APIの仕様・接続情報は未提示。仕様が決まるまで、この関数は実行すると止まる。
# 返す列は R/z0_utility/attributes.R の .attribute_columns_required_Curie を参照。
attribute_fetcher_aladdin <- function(isin, valuation_date) {
  # 認証情報の例：Sys.getenv("ALADDIN_API_TOKEN")（値は .Renviron に置き、コミットしない）
  stop("Aladdin APIの属性取得は未実装です（API仕様の提示後に実装）。", call. = FALSE)
}

# ---- 実行例 -----------------------------------------------------------------
# workflows_offer_sheet_JPY(
#   offer_path = "data/offers/jpy_offers.xlsx",
#   ois_path = "data/market/ois_JPY.txt",
#   jgb_path = "data/market/jgb_prices.txt",
#   valuation_date = valuation_date,
#   output_path = "output/offer_results_JPY.xlsx",
#   attribute_fetcher = attribute_fetcher_aladdin,
#   offer_settings = jpy_offer_settings, ois_settings = ois_settings,
#   jgb_settings = jgb_settings, pricing_settings = pricing_settings
# )
#
# workflows_offer_sheet_ISO(
#   offer_path = "data/offers/iso_offers.xlsx",
#   ois_paths = c("data/market/ois_USD.txt", "data/market/ois_EUR.txt", "data/market/ois_JPY.txt"),
#   basis_path = "data/market/ccs_basis.txt",
#   fx_path = "data/market/fx_spot.txt",
#   valuation_date = valuation_date,
#   output_path = "output/offer_results_ISO.xlsx",
#   attribute_fetcher = attribute_fetcher_aladdin,
#   offer_settings = iso_offer_settings, ois_settings = ois_settings,
#   basis_settings = basis_settings, fx_settings = fx_settings, pricing_settings = pricing_settings
# )
