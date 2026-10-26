# c1 TRSターム条件の抽出指示（Claude 用）

指示書の版：`c1-extract-v1`（抽出JSONの `document.instruction_version` に書く）

あなたは、TRS（トータル・リターン・スワップ）の確認書・ターム条件PDFを読み、下の項目を抽出します。
TRSの価格計算はしません。**値は必ずPDFの記載を根拠にし、推測で補いません。**

## 守ること

1. **PDFに書かれていることだけを抽出する。** 一般的な市場慣行や他の契約から推測しない。
2. **空欄・選択肢を区別する。** 値ごとに `value_status` を付ける。
   - `stated`：値が記載されている
   - `blank`：記入欄はあるが空欄（`[ ]`、`[]`、`______`、`MMMM DD, YYYY`、`[dd mm yyyy]`、`XXXX` など）
   - `bracketed`：角括弧の選択肢・既定案で、確定していない（例：`[Modified Following]`、`[3] months`、`[Preceding / Modified Following / Following]`）
   - `not_found`：文書に該当する記載がない
   - `stated` 以外は `value` を `null` にし、原文の空欄・選択肢は `quote` にそのまま書く
3. **参照資産と資金調達の参照金利を混同しない。** 参照資産・指数は `return.*`、資金調達レッグの変動金利は `funding.floating_rate_index`。
4. **評価日とフィキシング日を混同しない。** 参照資産の評価日は `return.valuation_*`、資金調達金利のフィキシング・リセットは `funding.fixing_*`／`funding.reset_*`。
5. **旧参照金利はそのまま。** 古い資料に `USD-LIBOR-BBA` などがあっても、現行金利（SOFR等）へ置き換えず、原文どおり抽出する。
6. **根拠を残す。** すべての項目に `page`（PDFのページ番号。1から数える）と `quote`（原文からの連続した抜き出し。改変しない）を付ける。`not_found` の項目は `page` と `quote` を `null` にしてよい。
7. **値の書き方をそろえる。**
   - 日付：`YYYY-MM-DD`
   - 数値：記号・単位・桁区切りなし（例：`1000000000`）。スプレッドは bp（0.35% → `35`）、下限は %（ゼロ → `0`）
   - コード項目：下の一覧の値を使う（例：頻度は `MONTHLY`／`QUARTERLY`／`SEMIANNUAL`／`ANNUAL`／`TERM`）
8. **確信度** `confidence` を `high`／`medium`／`low` で付け、迷った点は `note` に書く。
9. **全項目を出力する。** 該当がない項目も `not_found` として行を出す。

## 出力形式（1つのPDFにつき1つのJSON）

```json
{
  "document": {
    "document_id": "SC_TRS_draft",
    "file": "SC_Total-Return-Swap.pdf",
    "dealer": "Standard Chartered Bank",
    "pages": 17,
    "pdf_sha256": "（PDFのSHA-256。分かる場合）",
    "extracted_by": "claude",
    "model": "（モデル名）",
    "instruction_version": "c1-extract-v1",
    "extracted_at": "2026-10-26T10:00:00+09:00"
  },
  "fields": [
    {"field": "funding.floating_rate_index", "value": "USD-LIBOR-BBA", "value_status": "stated",
     "page": 5, "quote": "USD-LIBOR-BBA", "confidence": "high", "note": "原文は「the higher of (i) USD-LIBOR-BBA; and (ii) zero」"},
    {"field": "funding.spread_bp", "value": null, "value_status": "blank",
     "page": 5, "quote": "Plus [ ]%", "confidence": "high", "note": ""}
  ]
}
```

`extracted_by` は `claude`。

## 抽出する項目

| 項目ID | 内容 | 値の型・コード |
|---|---|---|
| `trade.product_type` | 商品の種類。文書が自分を何の取引と書いているか（例「Transaction Type: Total Return Swap」）。TRSでなければ `OTHER` とし、note に原文の取引名を書く | `TOTAL_RETURN_SWAP`／`OTHER` |
| `trade.dealer` | 証券会社 | 文字 |
| `trade.dealer_reference` | 証券会社の取引番号 | 文字 |
| `trade.trade_date` | 約定日 | 日付 |
| `trade.effective_date` | 効力発生日（開始日） | 日付 |
| `trade.termination_date` | 終了日 | 日付 |
| `trade.notional_amount` | 想定元本 | 数値 |
| `trade.notional_currency` | 想定元本の通貨 | 通貨コード |
| `trade.total_return_receiver` | トータル・リターンの受取者 | 文字（当事者名） |
| `trade.total_return_payer` | トータル・リターンの支払者 | 文字（当事者名） |
| `trade.calculation_agent` | 計算代理人 | 文字 |
| `trade.isda_definitions` | 準拠するISDA定義 | 文字（原文の名称。例 `2006 ISDA Definitions`） |
| `trade.fx_initial_spot_rate` | 当初スポットレート（為替） | 数値 |
| `trade.fx_rate_quotation` | 為替レートの表し方 | 文字（例 `IDR per USD`） |
| `trade.fx_hedge` | 為替ヘッジの有無・条件 | 文字 |
| `return.underlyer_type` | 参照資産の種類 | `INDEX`／`BOND`／`EQUITY` |
| `return.underlyer_name` | 参照資産・指数の名称 | 文字 |
| `return.underlyer_id` | 参照資産・指数の識別子 | 文字 |
| `return.underlyer_id_type` | 識別子の種類 | `ISIN`／`CUSIP`／`INDEX_CODE` |
| `return.underlyer_currency` | 参照資産の通貨 | 通貨コード |
| `return.underlyer_issuer` | 参照債券の発行体 | 文字 |
| `return.underlyer_maturity_date` | 参照債券の満期日 | 日付 |
| `return.return_type` | リターンの種類 | `TOTAL`／`PRICE` |
| `return.initial_price` | 当初価格 | 数値 |
| `return.valuation_frequency` | 評価の頻度 | `MONTHLY`／`QUARTERLY`／`SEMIANNUAL`／`ANNUAL`／`TERM`（最終のみ） |
| `return.valuation_date_rule` | 評価日の決め方 | 文字（原文の要約） |
| `return.valuation_calendar` | 評価日の営業日カレンダー | 地名（例 `New York, London`） |
| `return.final_valuation_date` | 最終評価日 | 日付 |
| `return.valuation_time` | 評価時刻 | 文字 |
| `return.income_treatment` | 利息・分配金の扱い | 文字 |
| `funding.floating_rate_index` | 資金調達の参照金利 | 文字（原文の名称のまま） |
| `funding.index_tenor` | 参照金利の期間 | `1M`／`3M`／`6M` など |
| `funding.spread_bp` | スプレッド | 数値（bp） |
| `funding.rate_floor_pct` | 参照金利の下限 | 数値（%） |
| `funding.day_count` | 日数計算方式 | 文字（原文。例 `Actual/360`） |
| `funding.compounding_method` | 複利・平均の方法 | `COMPOUNDING`／`AVERAGING`／`NONE` |
| `funding.payment_frequency` | 支払の頻度 | `MONTHLY`／`QUARTERLY`／`SEMIANNUAL`／`ANNUAL`／`TERM` |
| `funding.reset_frequency` | リセットの頻度 | 同上 |
| `funding.reset_relative_to` | リセット日の基準 | `PERIOD_START`／`PERIOD_END` |
| `funding.fixing_offset_days` | フィキシング日（リセット日からの営業日数。前は負） | 整数 |
| `funding.fixing_calendar` | フィキシングの営業日カレンダー | 地名 |
| `funding.fixing_business_day_convention` | フィキシング日の営業日調整 | 文字（原文。例 `Modified Following`） |
| `funding.lookback_days` | ルックバック（営業日） | 整数 |
| `funding.observation_shift` | 観測期間シフトの有無 | `YES`／`NO` |
| `funding.payment_lag_days` | 支払ラグ（営業日） | 整数 |
| `funding.payment_business_day_convention` | 支払日の営業日調整 | 文字（原文） |
| `funding.payment_calendar` | 支払日の営業日カレンダー | 地名 |

この一覧は `R/c1_trs_terms/trs_fields.R` の `trs_field_dictionary_c1()` と同じ。項目を増やすときは両方を直す。
