# ダミー市場データを作成する。すべて架空の値で、実際の市場価格ではない。
# 市場日は1つに統一し、標準のデモ形式はパイプ区切り（|）・UTF-8で書く。
# 取得時刻は quote_time 列（ISO 8601、日本時間）に入れる。
#
# 単位と向きの約束
# - OIS ：rate_pct はパーセント表示（1.25 は 1.25%）。通貨ごとに別ファイル。
#         LagrangeFinance の import_market_data() と instance_swap_curve() がそのまま受け取れる列
# - JGB ：時価ファイル1つに、単価（clean_price、額面100当たりのクリーン価格）と
#         利回り計算に必要な属性（coupon_rate、coupon_frequency、maturity_date、redemption）を持つ。
#         利回りは入力しない
# - FX  ：fx_spot は「外貨1単位当たりの円」（USDJPY 148.50 = 1米ドル 148.50円）。spot_date は受渡日
# - 通貨ベーシス：basis_bp は bp。円レッグに加算する。
#         取引は「外貨OISをフラットで支払い、円TONAR + basis_bp を受け取る」CCS。
#         負の値は、円レッグの受取がTONARを下回ることを意味する。
#         （LagrangeFinance の compute_asset_swap_basis_JPY() の basis_bp と同じ定義）
#         気配は償還日（maturity_date）ごとに複数の気配元から与える。

.demo_source_note_Curie <- "架空データ（Curieデモ用）。実際の市場価格ではない"
.demo_quote_time_Curie <- function(marketdate, time) paste0(format(marketdate), "T", time, "+09:00")

demo_ois_quotes_Curie <- function(marketdate = demo_marketdate_Curie) {
  tenors <- c("1M", "3M", "6M", "1Y", "2Y", "3Y", "4Y", "5Y", "7Y", "10Y", "12Y", "15Y", "20Y", "30Y")
  rates <- list(
    JPY = c(0.73, 0.76, 0.80, 0.88, 1.00, 1.10, 1.19, 1.27, 1.42, 1.62, 1.74, 1.88, 2.05, 2.20),
    USD = c(3.90, 3.85, 3.75, 3.60, 3.42, 3.38, 3.40, 3.45, 3.55, 3.70, 3.78, 3.88, 3.95, 3.90),
    EUR = c(1.92, 1.93, 1.95, 1.98, 2.05, 2.12, 2.20, 2.28, 2.42, 2.60, 2.70, 2.82, 2.90, 2.85)
  )
  index <- c(JPY = "TONAR", USD = "SOFR", EUR = "ESTR")
  times <- c(JPY = "15:00:00", USD = "17:00:00", EUR = "17:00:00")
  purrr::imap(rates, function(r, ccy) {
    tibble::tibble(
      marketdate = marketdate, quote_time = .demo_quote_time_Curie(marketdate, times[[ccy]]),
      currency = ccy, instrument = paste0(index[[ccy]], "_OIS"), instrument_type = "spot_ois",
      tenor = tenors, rate_pct = r, quote_side = "mid", source = .demo_source_note_Curie
    )
  }) |> purrr::list_rbind()
}

# JGB時価：オファーと同じ受渡日（T+2）に対応する単価を、架空の複利利回りから
# GaussQuant::bond_price_from_compound_yield_YEN_GQL() で生成する（小数3桁切捨て）。
# 実データでは受渡日T+1の単価をT+2の単価として扱わない（受渡日が違えば計算しない）。
demo_jgb_file_Curie <- function(marketdate = demo_marketdate_Curie) {
  settlement <- marketdate + 2L
  # 架空の複利利回り（%）。JGB05 と JGB08 は同じ償還日だが利回りを少し変える
  assumed_yield <- c(0.90, 1.05, 1.17, 1.27, 1.40, 1.60, 2.30, 1.42)
  demo_jgbs_Curie() |>
    dplyr::mutate(
      clean_price = purrr::pmap_dbl(
        list(.data$maturity_date, .data$coupon_rate, assumed_yield, .data$coupon_frequency),
        \(maturity, coupon, y, freq) GaussQuant::bond_price_from_compound_yield_YEN_GQL(
          start_date = as.character(settlement), end_date = as.character(maturity),
          coupon_rate = coupon * 100, compound_yield = y, redemption = 100,
          coupon_frequency = freq, compound_frequency = 2
        )
      )
    ) |>
    dplyr::transmute(
      marketdate = marketdate, quote_time = .demo_quote_time_Curie(marketdate, "15:00:00"),
      ISIN = .data$ISIN, security_name = .data$security_name,
      clean_price = .data$clean_price, price_type = "clean", settlement_date = settlement,
      coupon_rate = .data$coupon_rate, coupon_frequency = .data$coupon_frequency,
      issue_date = .data$issue_date, maturity_date = .data$maturity_date, redemption = .data$redemption,
      quote_side = "mid", source = .demo_source_note_Curie
    )
}

demo_fx_spot_Curie <- function(marketdate = demo_marketdate_Curie) {
  tibble::tibble(
    marketdate = marketdate, quote_time = .demo_quote_time_Curie(marketdate, "15:00:00"),
    currency_pair = c("USDJPY", "EURJPY"),
    base_currency = c("USD", "EUR"),   # 1単位を数える通貨（外貨）
    quote_currency = "JPY",            # 価格を表す通貨（円）
    fx_spot = c(148.50, 161.20),
    quote_convention = "JPY per 1 unit of base_currency",
    spot_date = marketdate + 2L,
    source = .demo_source_note_Curie
  )
}

# 通貨ベーシス気配。償還日ごとに2つの気配元を持ち、償還日が同じ気配は後で算術平均する。
# USDJPYには契約条件の違う気配（元本リセット型）も含め、平均の対象外になることを確かめる。
demo_ccs_basis_Curie <- function(marketdate = demo_marketdate_Curie) {
  maturities <- as.Date(c("2027-10-08", "2028-10-10", "2029-10-09", "2031-10-08", "2033-10-11", "2036-10-08"))
  base <- list(USD = c(-25, -30, -33, -35, -33, -30), EUR = c(-20, -24, -27, -29, -28, -26))
  index <- c(USD = "SOFR", EUR = "ESTR")
  constant <- purrr::imap(base, function(b, ccy) {
    tidyr::expand_grid(
      tibble::tibble(maturity_date = maturities, mid = b),
      tibble::tibble(quote_source = c("DEMO SOURCE 1", "DEMO SOURCE 2"), offset = c(-0.5, 0.5))
    ) |>
      dplyr::transmute(
        marketdate = marketdate, quote_time = .demo_quote_time_Curie(marketdate, "16:00:00"),
        currency_pair = paste0(ccy, "JPY"), currency = ccy,
        maturity_date = .data$maturity_date, basis_bp = .data$mid + .data$offset,
        quote_source = .data$quote_source,
        spread_leg = "JPY",
        pay_leg = paste0(ccy, " ", index[[ccy]], " flat"),
        receive_leg = "JPY TONAR + basis_bp",
        notional_type = "constant",       # 元本固定型（MtMリセットなし）
        collateral_currency = "JPY"
      )
  }) |> purrr::list_rbind()
  mtm <- constant |>
    dplyr::filter(.data$currency == "USD", .data$quote_source == "DEMO SOURCE 1") |>
    dplyr::slice(1:3) |>
    dplyr::mutate(notional_type = "mtm_reset", basis_bp = .data$basis_bp - 2, quote_source = "DEMO SOURCE 3")
  dplyr::bind_rows(constant, mtm) |> dplyr::mutate(source = .demo_source_note_Curie)
}

# 市場データを書き出し、パスを名前付きで返す（OISは通貨ごとに別ファイル）
create_demo_market_data_Curie <- function(dir, marketdate = demo_marketdate_Curie) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  stamp <- format(marketdate, "%Y%m%d")
  ois <- demo_ois_quotes_Curie(marketdate)
  data <- c(
    ois |> split(ois$currency) |> rlang::set_names(\(x) paste0("ois_", x)),
    list(jgb = demo_jgb_file_Curie(marketdate), fx = demo_fx_spot_Curie(marketdate),
         basis = demo_ccs_basis_Curie(marketdate))
  )
  paths <- purrr::imap_chr(data, function(x, name) {
    path <- file.path(dir, paste0("demo_", name, "_", stamp, ".txt"))
    readr::write_delim(x, path, delim = "|", na = "")
    path
  })
  writeLines(c(
    "Curie デモ市場データ（すべて架空。実際の市場価格ではない）",
    paste0("市場日: ", format(marketdate), "（quote_time は取得時刻、日本時間）"),
    "形式: パイプ区切り（|）、UTF-8、1行目が列名",
    "OIS  : 通貨ごとに別ファイル。rate_pct はパーセント。instrument_type = spot_ois",
    "JGB  : 時価ファイル。clean_price は額面100当たりのクリーン単価（受渡日 settlement_date = T+2）と、",
    "       利回り計算に必要な属性（coupon_rate、coupon_frequency、maturity_date、redemption）。利回りは入力しない",
    "       受渡日の違う単価（例：T+1）は、オファー受渡日（T+2）の比較には使わない",
    "FX   : fx_spot は base_currency 1単位当たりの円。spot_date は受渡日",
    "CCS  : basis_bp は bp。円レッグに加算（外貨OISフラット支払／円TONAR + basis 受取）",
    "       notional_type = constant は元本固定型。mtm_reset は元本リセット型（現行モデルの対象外）",
    "       同じ通貨ペア・市場日・契約条件・償還日の気配は算術平均して使う"
  ), file.path(dir, "README_market_data.txt"))
  paths
}

# 取込設定の確認用：同じOIS気配を、カンマ区切り・CP932・日本語列名で書く
write_demo_ois_alternative_Curie <- function(path, currency, marketdate = demo_marketdate_Curie) {
  x <- demo_ois_quotes_Curie(marketdate) |>
    dplyr::filter(.data$currency == .env$currency) |>
    dplyr::transmute(市場日 = format(.data$marketdate, "%Y-%m-%d"), 取得時刻 = .data$quote_time,
                     通貨 = .data$currency, 銘柄 = .data$instrument, 種別 = .data$instrument_type,
                     期間 = .data$tenor, `金利(%)` = .data$rate_pct)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  text <- readr::format_csv(x)
  con <- file(path, open = "wb")
  on.exit(close(con))
  writeBin(iconv(text, from = "UTF-8", to = "CP932", toRaw = TRUE)[[1]], con)
  invisible(path)
}

# 上のファイルを読むための取込設定
demo_ois_alternative_settings_Curie <- function() {
  text_import_settings_Curie(
    delim = ",", encoding = "CP932",
    column_map = c(marketdate = "市場日", quote_time = "取得時刻", currency = "通貨",
                   instrument = "銘柄", instrument_type = "種別", tenor = "期間", rate_pct = "金利(%)")
  )
}
