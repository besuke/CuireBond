# scripts：デモの生成と実行

本体（`R/`）とは分離したデモ用のスクリプト。データはすべて架空のテスト用。
Curieプロジェクトの最上位フォルダで実行する。

| ファイル | 役割 |
|---|---|
| `run_demo_JPY.R` | デモデータを生成し、JPYワークフローだけを実行 |
| `run_demo_ISO.R` | デモデータを生成し、ISOワークフローだけを実行 |
| `run_demo_checks.R` | JPY・ISOの実行、別パス・別形式の入力、市場日の不一致、上書き防止の確認 |
| `demo_data/` | 架空のオファーExcel・銘柄属性・市場データの生成、デモ属性取得関数 |

```r
source("scripts/run_demo_JPY.R", encoding = "UTF-8")
source("scripts/run_demo_ISO.R", encoding = "UTF-8")
source("scripts/run_demo_checks.R", encoding = "UTF-8")
```
