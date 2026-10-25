# CURIE

債券運用の分析・入れ替え提案・発注支援を目指す R Project。
現在は A1（オファーシートの取込・スプレッド計算・Excel出力）を実装している。
現在の実装状況と今後の構想（A1〜A4、B1〜B3）は **[docs/development_plan.md](docs/development_plan.md)** を参照。

| 番号 | 内容 | 状態 |
|---|---|---|
| A1 | オファーシート作成 | デモ実装済み・本番連携は未完了 |
| A2 | 時価適正性の検証 | 未実装 |
| A3 | クレジットレポート読込・紐づけ | 未実装 |
| A4 | 日次レポート・目玉オファー | 未実装 |
| B1 | ファンド戦略・制約条件の読込 | 未実装 |
| B2 | ポートフォリオ入れ替え提案 | 未実装 |
| B3 | Aladdin APIによる発注 | 未実装 |
| C1 | TRSターム条件の抽出結果 → 照合 → 確認表・FpML | デモ実装済み・本番連携は未完了（Aladdin用JSONは未実装） |
| Z0 | 共通関数 | A1が使う範囲をデモ実装済み |

A1 の本番連携で未完了の内容：

- Aladdin APIによる銘柄属性取得（現在はデモの属性取得関数のみ）
- 実際の入力ファイル（オファーExcel、OIS、JGB時価、FX、ベーシス）への対応・確認
- ASW翌日物レッグの実取引条件の確定（現在は半年払い・支払ラグ0をデモの仮定として使用）

A1 の2つのワークフロー：

- **JPY**：`workflows_offer_sheet_JPY()`。JPY建て（ユーロ円債を含む）。JPY OIS ASW と JGBスプレッド
- **ISO**：`workflows_offer_sheet_ISO()`。円以外の建て通貨。自国通貨OIS ASW と 円換算ASW

JPYとISOは別々に実行でき、結果は別のExcelに出力する。入力ファイル・評価日・出力先・取込設定・
銘柄属性の取得関数はすべて引数で渡す。

## 実行方法（デモ）

RStudio／Positron で `CURIE.Rproj` のフォルダを開き、Rコンソールで次を実行する。

```r
source("scripts/run_demo_JPY.R", encoding = "UTF-8")     # JPYのみ
source("scripts/run_demo_ISO.R", encoding = "UTF-8")     # ISOのみ
source("scripts/run_demo_checks.R", encoding = "UTF-8")  # 両方＋別パス・別形式・日付不一致などの確認
source("scripts/run_demo_c1_TRS.R", encoding = "UTF-8")  # C1：TRSターム条件の照合・確認表・FpML
```

架空のデモデータを `data/demo/` に作り、結果を `output/` に出力する。

## A1 POC（Quarto）

`docs/A1_offer_sheet_POC.qmd` は、既存のA1ワークフローを実際に実行し、入力・計算条件・結果・制約を
HTMLで確認するPOC。計算はA1の関数をそのまま呼び出し、表はワークフロー関数の戻り値から flextable で作る
（出力Excelは読み直さない）。データは架空のデモデータ。

```powershell
quarto render C:/AnalyticFin/Projects/CURIE/docs/A1_offer_sheet_POC.qmd
```

HTMLは自己完結型で、QMDと同じフォルダに出力される。デモ入力は `data/demo_poc/`、出力Excelは `output/poc/` に作る。

**CURIEルートの指定**（優先順位の高い順）：

1. Quarto の params：`quarto render <QMD> -P curie_root:C:/AnalyticFin/Projects/CURIE`
2. 環境変数：`$env:CURIE_ROOT = "C:/AnalyticFin/Projects/CURIE"`
3. QMDの場所の上位フォルダから、`CURIE.Rproj` と `R/load_CURIE.R` のあるフォルダを探索
4. 作業ディレクトリの上位フォルダから同様に探索

CURIE内の別フォルダへ移した場合は、そのまま `quarto render` で動く（3で見つかる）。CURIEの外へ移した場合は、
1か2で指定する。指定したフォルダがCURIEルートでない場合と、どこにも見つからない場合は、指定方法を示して停止する。

注意：QMDを置いたフォルダの上位に別の `_quarto.yml` がある場合、Quarto はそのプロジェクトの一部として扱う
（例：ユーザーフォルダ直下に `_quarto.yml` があると、その配下ではレンダリングが失敗する）。CURIEの外へ移すときは、
上位に `_quarto.yml` がないフォルダに置く。

実データへの差し替えは、POCの「実データへ差し替える方法」の表と、`config/settings_example.R` を参照
（`offer_path`、`ois_path(s)`、`jgb_path`、`basis_path`、`fx_path`、各取込設定、`valuation_date`、
`attribute_fetcher`、`pricing_settings`、`output_path` を差し替える）。

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

属性取得関数が返す列は `R/z0_utility/attributes.R` を参照。

## フォルダ構成

```
R/
  load_CURIE.R        本体の読込（Z0 → A1）
  a1_offer_sheet/     Phase 1：オファーシート作成
  a2_valuation/       Phase 1：時価適正性の検証（未実装。READMEのみ）
  a3_credit_reports/  Phase 1：クレジットレポート読込・紐づけ（未実装。READMEのみ）
  a4_daily_report/    Phase 1：日次レポート・目玉オファー（未実装。READMEのみ）
  b1_fund_strategy/   Phase 2：ファンド戦略・制約条件の読込（未実装。READMEのみ）
  b2_rebalancing/     Phase 2：ポートフォリオ入れ替え提案（未実装。READMEのみ）
  b3_aladdin_orders/  Phase 2：Aladdin APIによる発注（未実装。READMEのみ）
  c1_trs_terms/       C1：TRSターム条件の抽出結果の照合・確認表・FpML
  z0_utility/         共通関数（lagrange/ に Part II の移植関数）
scripts/              デモの生成（demo_data/）と実行スクリプト
config/               実データ用の設定のサンプル
docs/                 開発計画、仕様書との差分、A1 POC（A1_offer_sheet_POC.qmd）
data/                 入力データ（Git管理対象外）
output/               出力結果（Git管理対象外）
```

各フォルダの役割・予定する入出力は、そのフォルダの README.md を参照。

## Excelの共通書式

CURIEが作るすべてのExcel（A1の結果・標準形式のオファー・デモのオファー・C1の確認表など）は、
`R/z0_utility/excel_format.R` の `write_xlsx_CURIE()` で書き、次のフォントにする。

- 英字・数字：**Cambria**
- 日本語：**MS UI Gothic**

日本語と英数字が同じセルに混在する場合は、文字種ごとの区間（リッチテキスト）に分けてフォントを付ける
（例「テスト円債A 0.50% 2028/9」→「テスト円債」は MS UI Gothic、「A 0.50% 2028/9」は Cambria）。
数値・日付・英数字だけのセルは Cambria。

書式付きExcelのパッケージ openxlsx2 は、DLLが Windows のアプリケーション制御（スマートアプリコントロール）に
阻止されて使えないため、writexl で書いた後に xlsx 内の XML（`styles.xml`・`sharedStrings.xml`）を
xml2 で書き換えている。値・列構成は writexl だけで書いた場合と同じ。見出し行の太字はなし。

## 依存パッケージとセットアップ

R 4.6.0（動作確認環境：Windows 11、R 4.6.0 ucrt）。

| パッケージ | 用途 | 入手先 |
|---|---|---|
| dplyr, purrr, tibble, tidyr, stringr, readr, readxl | データ処理・取込（tidyverse） | CRAN |
| writexl | Excel出力 | CRAN |
| flextable, withr | A1 POC（Quarto）の表示 | CRAN |
| collapse | 移植した `merge_offer_bond_attributes()` が使用 | CRAN |
| QuantLib | カーブ・ASW計算 | R用QuantLibバインディング |
| GaussQuant | 債券生成・円債利回り | GitHub `besuke/GaussQuant` |

```r
install.packages(c("tidyverse", "writexl", "collapse", "flextable", "withr"))   # POCには Quarto も必要
```

QuantLib と GaussQuant は別途導入する。実データ用の設定は `config/settings_example.R` を
`config/settings_local.R` にコピーして書き換える（`*_local.R` と `.Renviron` はGit管理対象外）。
APIキー・認証情報はリポジトリに含めない。

Windowsでスマートアプリコントロールが有効な場合、CRANの一部パッケージのDLL（cli など）が
読み込みを阻止されることがある。セキュリティ設定は変更せず、阻止されたパッケージを確認して対応する。

GaussQuant は当面インストール済み版を使う。2026-10-25 時点で、インストール済み版は
2026.9.12.1（GitHub `fa721b2`）、ソース版（`C:/AnalyticFin/Projects/GaussQuant`）は 2026.06.18.1 で、
版番号は異なる。CURIEが使う関数（`R/z0_utility/environment_info.R` の一覧）は本体・引数ともに一致することを
ワークフローの実行ごとに確認し、Excelの「実行環境」「GaussQuant照合」シートに残す。

パッケージを入れ替え・更新した後は、Rセッションを再起動してから実行する。

## 次期機能（未実装）：オファーシートとクレジットレポートのURL連携

開発計画の A3（[docs/development_plan.md](docs/development_plan.md)、`R/a3_credit_reports/`）の一部。
出力Excelのオファー行から、対応するクレジットレポートを開けるようにする。

- **銘柄単位**：ISINをキーに、銘柄とクレジットレポートのURLを関連付ける。
- **発行体単位**：発行体単位のレポートは、発行体IDを介して銘柄（ISIN）と関連付ける。
- **Excelからの参照**：出力Excelのオファー行から、対応するレポートを開けるようにする。
- **複数レポート**：1つのオファーに複数のレポートがある場合は、別シートにレポートの一覧を作る。
- **保持する項目**：レポート名、発行元、発行日、URL。
- **URLがない場合**：空欄とし、推測でURLを生成しない。

レポート情報の入手元・形式、発行体IDの体系は未定。実装は別途着手する。

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
