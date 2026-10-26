# ダミーのオファーExcel（JPY用・ISO用）を作成する。
# JPYとISOでシート名・列名・見出し行の位置・日付と数量の書式をあえて変え、
# 変換関数が固定のシート名や列位置に依存していないことを確かめる。
# オファー単価（offer_price）と発行単価（issue_price）は別項目である。
# JPYシートの「参考:発行価格」は業者が添えた参考値で、評価には銘柄属性の値を使う。

demo_offer_rows_Curie <- function(marketdate = demo_marketdate_Curie) {
  bonds <- demo_bonds_Curie()
  # security_index は demo_bonds_Curie() の行番号。6（変動利付）と12（コーラブル）は未対応の確認用
  tibble::tribble(
    ~security_index, ~broker,                 ~offer_price, ~size_display,
    1L,              "テスト証券A",           99.62,        5,
    2L,              "テスト証券B",           98.15,        10,
    3L,              "テスト証券A",           98.70,        3,
    4L,              "テスト証券C",           97.95,        2,
    5L,              "テスト証券B",           100.45,       8,
    6L,              "テスト証券C",           100.10,       4,
    7L,              "DEMO DEALER X",         99.10,        2,
    8L,              "DEMO DEALER Y",         99.55,        5,
    9L,              "DEMO DEALER X",         101.20,       3,
    10L,             "DEMO DEALER Z",         99.80,        1.5,
    11L,             "DEMO DEALER Y",         99.40,        4,
    12L,             "DEMO DEALER Z",         100.60,       2
  ) |>
    dplyr::mutate(
      ISIN = bonds$ISIN[.data$security_index],
      security_name = bonds$security_name[.data$security_index],
      currency = bonds$currency[.data$security_index],
      issue_price = bonds$issue_price[.data$security_index],
      offer_date = marketdate,
      # デモでは全通貨とも約定日の2営業日後（祝日は考慮しない）
      settlement_date = marketdate + 2L
    )
}

# JPY用ブック：説明シート、オファーシート、訂正前シート（見出しが同じ＝候補が2つ）
write_demo_offer_jpy_Curie <- function(path, marketdate = demo_marketdate_Curie) {
  rows <- demo_offer_rows_Curie(marketdate) |> dplyr::filter(.data$currency == "JPY")
  # 日付は「YYYY/MM/DD」、数量は額面「億円」、単価は額面100円当たり
  jpy_sheet <- function(r, title) {
    header <- c("No.", "オファー日", "ISINコード", "銘柄名", "業者名",
                "単価", "数量(億円)", "受渡日", "参考:発行価格")
    body <- purrr::pmap(r, function(...) {
      x <- list(...)
      c(NA, format(x$offer_date, "%Y/%m/%d"), x$ISIN, x$security_name, x$broker,
        sprintf("%.2f", x$offer_price), format(x$size_display), format(x$settlement_date, "%Y/%m/%d"),
        sprintf("%.2f", x$issue_price))
    }) |> purrr::imap(function(v, i) { v[1] <- as.character(i); v })
    c(
      list(
        title,                                                   # 1行目
        paste0("作成日: ", format(marketdate, "%Y/%m/%d")),      # 2行目
        "単価は額面100円当たりのクリーン価格（経過利子を含まない）。数量は額面・億円。", # 3行目
        NA_character_,                                           # 4行目（空行）
        header                                                   # 5行目：見出し
      ),
      body,
      list(NA_character_, "※ 架空の銘柄・価格です。実在の銘柄・市場価格ではありません。")
    ) |> rows_to_sheet_Curie()
  }
  # 訂正前シート：価格だけ古い値（変換時に候補が2つになることを示すため）
  stale <- rows |> dplyr::mutate(offer_price = .data$offer_price - 0.10)
  sheets <- list(
    "説明" = description_sheet_Curie(c(
      "用途" = "Curieデモ用の円債オファー（架空データ）",
      "対象シート" = "「円債オファー」が正。「円債オファー_訂正前」は差替前の版",
      "見出し行" = "5行目（1〜4行目は表題と注記）",
      "単価" = "額面100円当たりのクリーン価格",
      "数量" = "額面金額、単位は億円",
      "日付" = "YYYY/MM/DD",
      "参考:発行価格" = "発行時の価格（参考値）。オファー単価とは別項目",
      "ISIN" = "TESTを含む架空のテスト用コード。実在しない"
    )),
    "円債オファー" = jpy_sheet(rows, "円債オファー一覧（デモ・架空データ）"),
    "円債オファー_訂正前" = jpy_sheet(stale, "円債オファー一覧（訂正前・使用しない）")
  )
  write_xlsx_Curie(sheets, path, col_names = FALSE)
  invisible(path)
}

# ISO（外貨）用ブック：英語の見出し、見出し行は3行目、日付はYYYYMMDD、数量は百万単位
write_demo_offer_iso_Curie <- function(path, marketdate = demo_marketdate_Curie) {
  rows <- demo_offer_rows_Curie(marketdate) |> dplyr::filter(.data$currency != "JPY")
  header <- c("Trade Date", "Settle Date", "ISIN", "Security", "Dealer", "Ccy", "Price", "Size (MM)")
  body <- purrr::pmap(rows, function(...) {
    x <- list(...)
    c(format(x$offer_date, "%Y%m%d"), format(x$settlement_date, "%Y%m%d"), x$ISIN,
      x$security_name, x$broker, x$currency, sprintf("%.2f", x$offer_price), format(x$size_display))
  })
  offers <- c(
    list(
      "Foreign currency bond offers - DEMO, fictitious data",
      "Price: clean, per 100 face. Size: face amount in millions of the bond currency."
    ),
    list(header),  # 3行目：見出し
    body
  ) |> rows_to_sheet_Curie()
  readme <- tibble::tribble(
    ~V1,              ~V2,
    "Item",           "Description",
    "Purpose",        "Curie demo offers for foreign currency (ISO) bonds - fictitious",
    "Header row",     "Row 3 of sheet 'Offers'",
    "Price",          "Clean price per 100 face",
    "Size",           "Face amount, millions of the bond currency (Size (MM))",
    "Dates",          "YYYYMMDD",
    "ISIN",           "Test codes containing TEST. They do not exist.",
    "注記",           "架空のテスト用データ。実在の銘柄・市場価格ではない"
  )
  write_xlsx_Curie(list("README" = readme, "Offers" = offers), path, col_names = FALSE)
  invisible(path)
}

create_demo_offers_Curie <- function(dir, marketdate = demo_marketdate_Curie) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  stamp <- format(marketdate, "%Y%m%d")
  c(
    JPY = write_demo_offer_jpy_Curie(file.path(dir, paste0("demo_offers_JPY_", stamp, ".xlsx")), marketdate),
    ISO = write_demo_offer_iso_Curie(file.path(dir, paste0("demo_offers_ISO_", stamp, ".xlsx")), marketdate)
  )
}
