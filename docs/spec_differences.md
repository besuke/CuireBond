# 仕様書（asw_offer_sheet_spec.qmd）とCURIE要件の差分

基準：`LagrangeFinance/docs/asw_offer_sheet_spec.qmd`（実装基準コミット `e955cf4`。
CURIEが移植した `LagrangeFinance/R` の関数はこのコミットと同一内容）。
記録日：2026-10-25。

## 1. 仕様書にない、CURIEで追加した要件

| 項目 | CURIEの扱い |
|---|---|
| ワークフローの分割 | `workflows_offer_sheet_JPY()`（JPY建て、ユーロ円債を含む）と `workflows_offer_sheet_ISO()`（円以外の建て通貨）に分け、別々に実行・出力する。仕様書は1つのワークシートで全通貨を扱う |
| 入力の引数化 | オファー・OIS・JGB・FX・ベーシスのパス、評価日、出力先、取込設定（区切り文字・文字コード・列名対応、シート指定）を引数にする |
| OISの区切り文字 | 仕様書（既存 `import_market_data()`）はパイプ区切りのみ。CURIEは他の区切り文字・文字コード・列名のファイルをパイプ区切りへ変換してから既存関数へ渡す |
| オファー形式の変換 | シート名・列名・見出し行が異なるExcelを見出しから判定し、既存 `import_offer_sheet()` の標準形式へ変換する。元ファイル名・シート名・行番号を残す |
| 銘柄属性の取得 | 仕様書はAladdinから書き出したRDSを読む。CURIEは属性取得関数（ISIN→属性）を引数で受け取り、取得結果をRDSに記録して既存関数へ渡す。API仕様は未提示のため、デモ取得関数のみ |
| 属性の項目 | 固定／変動区分、変動利付の参照金利・スプレッド・リセット条件、First Call日・コール価格を追加。最終償還日（`maturity_date`）とFirst Call日は別項目。計算終期（`calculation_end_date`）を明示 |
| 未対応商品 | 変動利付・期限前償還条項付き等は `pricing_status = unsupported` とし、固定利付として代用しない。仕様書は入力検証で固定利付以外を除外するのみ |
| 受渡日の一致 | 仕様書の課題（「受渡日を明示できるAPI、または債券生成条件との一致検証を追加する」）に対し、既存関数を変えずに一致検証を採用。不一致の行は計算しない |
| JGBスプレッド | 仕様書に定義なし。（対象JPY債の複利利回り − 同じ償還日に補間したJGB複利利回り）× 100 bp。JGB時価（単価＋属性）から `GaussQuant::bond_compound_yield_from_price_YEN_GQL()` で複利利回りを計算し、同一市場日・同一受渡日・同一償還日を算術平均して自然3次スプライン補間（外挿なし） |
| ベーシスの選択 | 仕様書は「満期に対応するCCS気配1点」。CURIEは同一の市場日・通貨ペア・契約条件・償還日の気配を算術平均し、自然3次スプラインで償還日の値を補間する（外挿なし）。これは気配の補間で、CCS期間構造の校正ではない |
| 市場日・取得時刻 | 評価日と同じ市場日のデータだけを使い、各データの市場日・取得時刻を出力する。不一致・不足は別の日付で補わない（仕様書の「過去の市場データによる自動補完は行わない」を全データへ拡張） |
| 計算状態 | 仕様書の将来要件 `pricing_status`・`pricing_message` を実装。success／partial／failed／unsupported／skipped |
| ASWの払い方 | 既存関数の既定値（半年払い・支払ラグ0）を、現段階ではデモの仮定として設定で変更可能にした |
| 出力 | JPY／ISOそれぞれ別のExcel。元のオファーExcelは上書きしない |

## 2. 仕様書の評価条件項目のうち、未実装のもの

仕様書「評価条件の必須項目」の `collateral_currency`、`collateral_rate_index`、`collateral_terms_id`、
`projection_curve_id`、`discount_curve_id`、`fx_curve_id`、`pricing_method`、`calibration_method` は、
個別の列としては未実装。現在は次で代替している。

- 使用カーブ：`native_ois_curve_id`、`jpy_ois_curve_id`（種類|ファイル|市場日|銘柄）
- FX・ベーシス：`fx_quote_id`、`basis_quote_set_id`、`basis_method`
- モデル：`jpy_asw_model`（既存の簡易モデルであることと制約）、`asw_conditions_note`

## 3. 既存モデルの制約（仕様書どおり、CURIEでも残るもの）

- 円換算ASWは円担保・元本固定・開始日と満期に元本交換・満期ベーシス1点の一定スプレッド校正による簡易モデル
- MtMリセット型CCS、CCS期間構造のブートストラップ、FXスポット受渡日とASW開始日がずれる場合のFXスワップ調整、
  取引費用、クオント調整、信用・資本・資金調達の調整は扱わない
- 対象は固定利付・期中償還なし・満期に額面100で償還する債券のみ
- オプション価値（コール・プット）は評価しない

## 4. 今後提示される情報で確定させるもの

- Aladdin APIの仕様・接続情報・項目名（属性取得関数の本番実装）
- 実データ（オファーExcel、OIS、JGB時価、FX、ベーシス）のファイル形式
- ASWの翌日物レッグの払い方（現在はデモの仮定）
