# CURIE

債券オファーシートの取込・スプレッド計算・Excel出力を行う R Project。

- **JPY**：`workflows_offer_sheet_JPY()`。JPY建て（ユーロ円債を含む）。JPY OIS ASW と JGBスプレッド
- **ISO**：`workflows_offer_sheet_ISO()`。円以外の建て通貨。自国通貨OIS ASW と 円換算ASW

JPYとISOは別々に実行でき、結果は別のExcelに出力する。入力ファイル・評価日・出力先・取込設定・
銘柄属性の取得関数はすべて引数で渡す。

## 実行方法（デモ）

RStudio／Positron で `CURIE.Rproj` のフォルダを開き、Rコンソールで次を実行する。

```r
source("run_demo_JPY.R", encoding = "UTF-8")     # JPYのみ
source("run_demo_ISO.R", encoding = "UTF-8")     # ISOのみ
source("run_demo_checks.R", encoding = "UTF-8")  # 両方＋別パス・別形式・日付不一致などの確認
```

架空のデモデータを `data/demo/` に作り、結果を `output/` に出力する。

## 本体の呼び出し例

```r
source("R/load_CURIE.R", encoding = "UTF-8")

workflows_offer_sheet_JPY(
  offer_path = "path/to/jpy_offers.xlsx",
  ois_path = "path/to/ois_JPY.txt",
  jgb_path = "path/to/jgb_prices.txt",
  valuation_date = "2026-10-06",
  output_path = "output/offer_results_JPY_20261006.xlsx",
  attribute_fetcher = my_attribute_fetcher,      # function(ISIN, valuation_date) -> 属性の表
  offer_settings = offer_import_settings_CURIE(sheet = "円債オファー", currency = "JPY")
)
```

属性取得関数が返す列は `R/attributes.R` を参照。

## フォルダ構成

- `R/lagrange/` : LagrangeFinance/R から移植した Part II の関数（計算ロジックは変更しない）
- `R/` : CURIE本体（オファー変換、取込設定、属性取得、検証、計算、Excel出力、ワークフロー）
- `R/demo/` : デモデータの生成とデモ属性取得関数（本体とは分離）
- `docs/spec_differences.md` : 仕様書との差分
- `config/settings_example.R` : 実データ用の設定のサンプル（取込設定・属性取得関数の雛形）
- `run_demo_*.R` : 架空のデモデータを再生成して実行するスクリプト
- `data/` : 入力データ（Git管理対象外）
- `output/` : 出力結果（Git管理対象外）

## 依存パッケージとセットアップ

R 4.6.0（動作確認環境：Windows 11、R 4.6.0 ucrt）。

| パッケージ | 用途 | 入手先 |
|---|---|---|
| dplyr, purrr, tibble, tidyr, stringr, readr, readxl | データ処理・取込（tidyverse） | CRAN |
| writexl | Excel出力 | CRAN |
| collapse | 移植した `merge_offer_bond_attributes()` が使用 | CRAN |
| QuantLib | カーブ・ASW計算 | R用QuantLibバインディング |
| GaussQuant | 債券生成・円債利回り | GitHub `besuke/GaussQuant` |

```r
install.packages(c("tidyverse", "writexl", "collapse"))
```

QuantLib と GaussQuant は別途導入する。実データ用の設定は `config/settings_example.R` を
`config/settings_local.R` にコピーして書き換える（`*_local.R` と `.Renviron` はGit管理対象外）。
APIキー・認証情報はリポジトリに含めない。

Windowsでスマートアプリコントロールが有効な場合、CRANの一部パッケージのDLL（cli など）が
読み込みを阻止されることがある。セキュリティ設定は変更せず、阻止されたパッケージを確認して対応する。

GaussQuant は当面インストール済み版を使う。2026-10-25 時点で、インストール済み版は
2026.9.12.1（GitHub `fa721b2`）、ソース版（`C:/AnalyticFin/Projects/GaussQuant`）は 2026.06.18.1 で、
版番号は異なる。CURIEが使う関数（`R/environment_info.R` の一覧）は本体・引数ともに一致することを
ワークフローの実行ごとに確認し、Excelの「実行環境」「GaussQuant照合」シートに残す。

パッケージを入れ替え・更新した後は、Rセッションを再起動してから実行する。

## 計算の前提と既知の制約

- **市場データの日付**：評価日と同じ市場日のデータだけを使い、不一致・不足を別の日付で補わない。
- **計算終期**：最終償還日。変動利付・期限前償還条項付き等は現在の計算関数の対象外で、
  固定利付債として代用せず `unsupported` と理由を出力する。
- **ASWの翌日物レッグ**：半年払い・支払ラグ0は、現段階ではデモの仮定。`pricing_settings_CURIE()` で変更できる。
- **JGBスプレッド**：（対象JPY債の複利利回り − 同じ償還日のJGB複利利回り）× 100 bp。
  両者とも `GaussQuant::bond_compound_yield_from_price_YEN_GQL()`、クリーン単価、同じ受渡日、利払・複利とも年2回で計算する。
  JGB利回りは同一市場日・同一受渡日・同一償還日を算術平均し、残存年数で自然3次スプライン補間する（外挿なし）。
  受渡日の違うJGB単価（例：T+1）は、T+2のオファーとの比較に使わない。
- **円換算ASW**：既存の簡易モデル（`compute_asset_swap_basis_JPY()`）。円担保・元本固定・開始日と満期に元本交換で、
  満期に対応するベーシス1点で外貨割引の一定スプレッドを校正する。
  ベーシスは同一条件・同一償還日の気配を平均して自然3次スプライン補間した値を使うが、
  これは**ベーシス気配の補間**であり、CCSの期間構造全体を校正したモデルではない。
  MtMリセット型CCS、FXスポットの受渡日とASW開始日がずれる場合のFXスワップ調整、取引費用、
  クオント調整、信用・資本・資金調達の調整は含まない。
