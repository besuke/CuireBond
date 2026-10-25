# z0_utility：共通関数

各フェーズ（A／B）から共通に使う関数。GaussQuant と QuantLib はパッケージとして利用し、関数をコピーしない。

| ファイル | 役割 |
|---|---|
| `lagrange/` | LagrangeFinance/R から移植した Part II の関数（計算ロジック・関数名は変更しない） |
| `00_dependencies.R` | 必要パッケージの確認、移植関数の読込（R標準の関数だけで書く） |
| `isin.R` | ISINのチェックディジット |
| `parse_utils.R` | 全角→半角、日付・数値の解釈 |
| `import_settings.R` | 取込設定（区切り文字・文字コード・列名対応）、テキストの読込、OISの読込、市場データの日付記録 |
| `market_data_import.R` | FXスポット、通貨ベーシス、JGB時価の読込 |
| `interpolation.R` | 償還日を横軸とした自然3次スプライン補間（外挿なし） |
| `bond_yield_YEN.R` | 円債の複利利回り（GaussQuant の円債関数） |
| `attributes.R` | 銘柄属性の取得関数の約束、計算終期の判定 |
| `bond_build.R` | 銘柄属性から固定利付債を生成（GaussQuant） |
| `curve_build.R` | 評価日の気配からOISカーブを構築（移植した `instance_swap_curve()`） |
| `environment_info.R` | 実行環境とGaussQuant（インストール済み版とソース版）の照合 |
| `report_format.R` | 表示用の整形（桁数・日付・欠損と未計算の区別、flextable の共通書式）。計算はしない |
