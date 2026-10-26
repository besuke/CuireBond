# c1_trs_terms：TRS 確認書 → Claude の読解 → 条件 JSON・FpML［実装済み・FpML の公式XSD 検証は未設定］

各社の TRS 確認書・ターム条件を Claude に読ませ、根拠（ページ・位置・原文の引用）つきの条件 JSON と FpML を作り、
R で検証する。TRS の価格計算はしない。Aladdin 用 JSON は仕様の提示後に追加する。

```
資料の URL（またはファイルのパス）をコンソールに貼る
          │
          ▼
   PowerShell（PS/run_document.ps1）
     1. 原資料の取得と記録（取得元・取得日時・HTTP・Content-Type・形式の判定根拠・SHA-256）
     2. 読解用データ（PDF はページごとの本文、文字のないページは Windows 標準の OCR、HTML は本文）
     3. Claude Code（claude -p）が原資料を読み、reviewed.json・fpml.xml・review_items.md を作る
     4. 成果物：result/conditions.json と result/fpml.xml
          │  終了コード
          ▼
   R：条件（定義・重複・必須・型・単位・日付の順序・商品の種類）、根拠（引用の実在）、版（SHA-256）、
      FpML（XML構文／公式XSD／条件との一致）を検証 → fpml_final.xml または fpml_DRAFT.xml
```

## 守ること

- **推測で埋めない**：値の状態（記載あり／空欄／選択肢（未確定）／記載なし）を区別する
- **区別すること**：参照資産・指数と資金調達の参照金利、参照資産の評価日と資金調達金利のフィキシング／リセット日
- **旧参照金利**：原文どおり（例：USD-LIBOR-BBA を SOFR に置き換えない）
- **根拠を確かめる**：Claude の引用が原資料の指定ページにあるかを R が独自に抜き出した本文で確かめる
- **最終版の条件**：検証にすべて合格し、必須項目がすべて原文で確認され、公式XSD にも適合したときだけ「検証済み」（fpml_final.xml）。
  それ以外は「要確認」（fpml_DRAFT.xml）
- **アクセス制限は回避しない**：取得できなければ「取得失敗」として理由を表示する

## 実行方法

```r
source("scripts/run_c1_document.R", encoding = "UTF-8")   # 「資料のURL（またはファイルのパス）を貼ってください:」
```

```
Rscript scripts/run_c1_document.R <URLまたはファイルのパス> [--claude=api|demo] [--claude-demo-dir=<フォルダ>] [--show-fpml]
Rscript scripts/test_c1_case_mock.R        # モックテスト（Claude を呼ばない。ネットワークにつながない）
pwsh -File scripts/test_c1_http_mock.ps1   # URL 取得のテスト（HTTP 応答のモック）
Rscript scripts/make_c1_walkthrough.R      # 動き方の説明 PDF（docs/c1_walkthrough.pdf）
```

- 入力：公開 PDF・HTML の URL、ローカルの PDF・HTML。Box の資料は Box Drive の同期フォルダのパスを貼る
- 公開元によっては名前と連絡先を含む User-Agent が必要（SEC など）：`.Renviron` に `CURIE_HTTP_USER_AGENT="氏名 メールアドレス"`
- Claude が PDF を読むには poppler（pdftoppm）が必要（winget の oschwartz10612.Poppler）
- FpML の公式XSD：`CURIE_FPML_XSD` に公式スキーマ一式のファイルを指定する（未設定なら「検証済み」にならない）

## 案件フォルダ（`output/c1_trs/cases/<案件ID>/`）

| フォルダ・ファイル | 内容 |
|---|---|
| `result/conditions.json` | **成果物**：条件（1項目1件。leg_id・seq で反復を区別）、資料・取得の記録、要確認事項 |
| `result/fpml_final.xml`／`fpml_DRAFT.xml` | **成果物**：FpML（検証済みなら final、それ以外は DRAFT） |
| `manifest.json` | 段階ごとの状態・終了コード・エラー |
| `r_validation.json` | R の検証結果 |
| `source/` | 原資料（そのまま）、取得記録、読解用データ |
| `agents/claude/` | Claude への依頼文・読解結果・FpML 案・実行記録 |
| `logs/run_log.jsonl` | 段階ごとのログ |

終了コード：0 成果物あり（R の検証待ち）、3 取得失敗、6 Claude 読解失敗、7 Claude 回答形式不正、8 読解用データ作成失敗、1 想定外のエラー

## ファイル

| ファイル | 役割 |
|---|---|
| `PS/run_document.ps1` | 入口（取得・読解用データ・Claude・成果物の保存） |
| `PS/common.ps1` | 共通処理（ログ、終了コード、取得、形式の判定、文字コード、本文抽出、OCR の呼び出し） |
| `PS/invoke_claude.ps1` | Claude Code の実行と、結果の形式チェック |
| `PS/ocr_windows.ps1` | Windows 標準の OCR（Windows PowerShell 5.1 で実行） |
| `trs_fields.R` | 抽出項目の定義、値の状態 |
| `verify_quotes.R` | 引用照合のための正規化、SHA-256 |
| `fpml_trs.R` | FpML の作成（デモ用） |
| `fpml_validate.R` | FpML の構文・スキーマ検証（xml2） |
| `read_results.R` | 案件フォルダの読込 |
| `validate_results.R` | 条件・根拠・版・FpML の検証 |
| `workflows_c1_case.R` | 1件の全工程（`workflows_c1_case()`） |

## 未完了

- FpML の公式XSD での検証、ISDA 定義の名称と FpML のコード（contractualDefinitions 等）の確認
- Aladdin 用 JSON への変換（仕様の提示後）
- 同じ資料を複数回読ませたときの揺れの測定、正解つき試験資料での正答率の測定
