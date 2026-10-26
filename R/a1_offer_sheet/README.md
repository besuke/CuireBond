# a1_offer_sheet：オファーシート作成（Phase 1）［デモ実装済み・本番連携は未完了］

JPY／ISOのオファーExcelを取り込み、銘柄属性を結合し、スプレッドを計算してExcelへ出力する。
架空のデモデータで動作を確認済み。

- `workflows_offer_sheet_JPY()`：JPY建て（ユーロ円債を含む）。JPY OIS ASW、JGBスプレッド
- `workflows_offer_sheet_ISO()`：円以外の建て通貨。自国通貨OIS ASW、円換算ASW（既存の簡易モデル）
- 入力ファイル・評価日・出力先・取込設定・属性取得関数はすべて引数で渡す

本番連携で未完了の内容：

- Aladdin APIによる銘柄属性取得（現在はデモの属性取得関数のみ）
- 実際の入力ファイル（オファーExcel、OIS、JGB時価、FX、ベーシス）への対応・確認
- ASW翌日物レッグの実取引条件の確定（現在は半年払い・支払ラグ0をデモの仮定として使用）

| ファイル | 役割 |
|---|---|
| `workflows_offer_sheet.R` | JPY／ISOのワークフロー本体 |
| `convert_offer_sheet.R` | オファーExcelの見出し判定と共通形式への変換 |
| `spread_inputs.R` | 通貨ベーシス・JGB利回りの平均と補間、JGBスプレッド |
| `validate_offer_worksheet.R` | 移植した `workflows_validate_worksheet()` による検証と、Curie独自の確認 |
| `compute_offer_pricing.R` | ASW・円換算ASW・JGBスプレッドの計算（オファー1行ごと） |
| `write_offer_results_excel.R` | 結果のExcel出力（JPY／ISO別） |
| `report_tables.R` | 結果をHTMLで確認するための表（flextable）。ワークフロー関数の戻り値から作り、計算はしない |

共通処理は `R/z0_utility/` を使う（表の書式は `R/z0_utility/report_format.R`）。

## POC（Quarto）

`docs/A1_offer_sheet_POC.qmd` で、このフォルダのワークフローを架空のデモデータで実行し、
入力ファイルと評価条件、件数、JPY／ISOの計算結果、未対応・失敗の理由、JGB利回りとベーシスの補間、
OISカーブの再現誤差、円換算ASWの残差、入力ファイルの保全をHTMLで確認できる。

- **目的**：既存のA1ワークフローを実行し、入力・計算条件・結果・制約を確認する（計算はQMD内で実装しない）
- **配置場所**：`docs/A1_offer_sheet_POC.qmd`（HTMLは同じフォルダに自己完結型で出力）
- **実行方法**：`quarto render C:/AnalyticFin/Projects/Curie/docs/A1_offer_sheet_POC.qmd`
- **ルートの指定**：params `curie_root` → 環境変数 `Curie_ROOT` → QMDの場所の上位を探索 → 作業ディレクトリの上位を探索。
  Curieの外へ移した場合は、`-P curie_root:<Curieのフォルダ>` か `Curie_ROOT` で指定する
- **実データへの差し替え**：POCの「実データへ差し替える方法」の表のとおり、ワークフロー関数の引数
  （入力パス、取込設定、評価日、属性取得関数、計算設定、出力先）を差し替える。計算処理は変えない
