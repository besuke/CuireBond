# c1_trs_terms：TRS 確認書 → R → Claude Code → R［実装済み・FpML の公式XSD 検証は未設定］

各社の TRS 確認書・ターム条件を Claude Code に読ませ、根拠（ページ・位置・原文の引用）つきの条件 JSON と FpML を作り、
R で検証する。処理はすべて R（PowerShell は使わない）。TRS の価格計算はしない。Aladdin 送信用 JSON は下書きだけ作る（送信先・認証が未提示のため送信しない）。

```
資料の URL（またはファイルのパス）をコンソールに貼る
          │
          ▼
   R：取得と記録（取得元・取得日時・HTTP・Content-Type・形式の判定根拠・文字コード・SHA-256）     acquire_document.R
   R：読解用データ（PDF はページごとの本文、文字のないページは「本文なし」と記録、HTML は本文） reading_data.R
          │
          ▼
   Claude Code（claude -p。R が processx で起動。タイムアウトあり）                                    claude_agent.R
     作業フォルダの原資料（document.pdf／document.htm）と指示書を読み、reviewed.json・fpml.xml・review_items.md を作る
          │
          ▼
   R：result/conditions.json と FpML を保存 → 検証（4つの判定）→ fpml_final.xml または fpml_DRAFT.xml   workflows_c1_case.R
        XML構文／公式XSD／原文根拠（引用）／条件 JSON との整合（FpML の値。レッグ・番号ごと）          validate_results.R
```

## 守ること

- **推測で埋めない**：値の状態（記載あり／空欄／選択肢（未確定）／記載なし）を区別する
- **区別すること**：参照資産・指数と資金調達の参照金利、参照資産の評価日と資金調達金利のフィキシング／リセット日
- **旧参照金利**：原文どおり（例：USD-LIBOR-BBA を SOFR に置き換えない）
- **原資料を直接読ませる**：Claude には取得した原資料を渡す（元の URL から再取得させない。ウェブ取得のツールは渡さない）
- **根拠を確かめる**：記載ありで確認済みの値には引用が必要（なければ「引用なし（要確認）」）。記載のない項目は「引用不要」。
  引用が原資料の指定ページにあるかを、R が抜き出した本文で確かめる
- **FpML の整合**：元本額・通貨・参照資産・参照金利・テナー・スプレッド・日数計算・リセット・フィキシング・カレンダー・
  営業日調整・支払頻度などを、レッグ（returnLeg／interestLeg）と番号ごとに比べる。別レッグの値や部分一致では合格にしない。
  要素がレッグ内に直接なく、FpML に明示された参照（例：interestLegPaymentDates の `dateRelativeTo href="returnLegPaymentDates"`）が
  あるときだけ、参照先の値で比べる（たどった参照を詳細に記録）。対応する要素がない項目は「未検証」
- **最終版の条件**：4つの判定がすべて合格し、必須項目がすべて原文で確認されたときだけ「検証済み」（fpml_final.xml）。
  それ以外は「要確認」（fpml_DRAFT.xml）
- **失敗を成功として扱わない**：Claude の失敗・タイムアウトでは、途中の結果ファイルを使わない（`.incomplete` として残す）
- **アクセス制限は回避しない**：取得できなければ「取得失敗」として理由を表示する

## 実行方法

```r
source("scripts/run_c1_document.R", encoding = "UTF-8")   # 「資料のURL（またはファイルのパス）を貼ってください:」
```

コンソールには、資料（PDF・HTML）から抜き出した条件の表と、XML（FpML）の保存先だけを表示する。
検証の詳細（4つの判定と各検証）は `r_validation.json` に保存する。

```
Rscript scripts/run_c1_document.R <URLまたはファイルのパス> [--claude=api|demo] [--claude-demo-dir=<フォルダ>]
Rscript scripts/test_c1_case_mock.R        # モックテスト（Claude を呼ばない。HTTP 応答もモック）
Rscript scripts/make_c1_walkthrough.R      # 動き方の説明 PDF（docs/c1_walkthrough.pdf）
```

- 入力：公開 PDF・HTML の URL、ローカルの PDF・HTML。Box の資料は Box Drive の同期フォルダのパスを貼る
- モード：`api`（Claude Code を実行）／`demo`（用意した結果を使う。Claude は呼ばない）。黙って切り替えない
- 公開元によっては名前と連絡先を含む User-Agent が必要（SEC など）：`tmp/.Renviron` に `CURIE_HTTP_USER_AGENT="氏名 メールアドレス"` と書く（`R/load_CURIE.R` が読み込む。`tmp/` は Git 管理対象外）
- Claude Code：`claude` を PATH から探す（`CURIE_CLAUDE_CMD` で指定可）。タイムアウトは `CURIE_CLAUDE_TIMEOUT_SEC`（既定 1800 秒）
- Claude が PDF を読むには poppler（pdftoppm）が必要（winget の oschwartz10612.Poppler。`CURIE_POPPLER_BIN` で指定可）
- FpML の公式XSD：`CURIE_FPML_XSD` に公式スキーマ一式のファイルを指定する（未設定なら「検証済み」にならない）

## 案件フォルダ（`output/c1_trs/cases/<案件ID>/`）

| フォルダ・ファイル | 内容 |
|---|---|
| `result/conditions.json` | **成果物**：条件（1項目1件。leg_id・seq で反復を区別）、資料・取得の記録、要確認事項 |
| `result/fpml_final.xml`／`fpml_DRAFT.xml` | **成果物**：FpML（検証済みなら final、それ以外は DRAFT） |
| `manifest.json` | 段階ごとの状態、失敗の種類と理由、取得・読解用データ・Claude の実行記録（終了コード・秒数・タイムアウト・費用の目安） |
| `r_validation.json` | R の検証結果（4つの判定と各検証） |
| `source/` | 原資料（そのまま）、取得記録、読解用データ |
| `agents/claude/` | Claude への依頼文・原資料の写し・結果・標準出力（`claude_stdout.txt`）・標準エラー（`claude_stderr.txt`） |
| `logs/run_log.jsonl` | 段階ごとのログ |
| `result/aladdin_request_DRAFT.json` | Aladdin 送信用の要求本文の下書き（未送信。未確定の項目は含めない。空なら `{}`） |
| `result/aladdin_unresolved.json` | 下書きであること（`status`）、送信先（未送信）、単位の換算（`conversions`）、未確定の項目と理由（`unresolved`） |

状態：検証済み／要確認／デモ／取得失敗／読解用データ作成失敗／読解失敗（Claude の失敗・タイムアウト・読めなかった）／回答形式不正

## Aladdin（payload の生成まで。送信はしない）

| 項目 | 内容 |
|---|---|
| 送信先 | `POST https://daiwa.blackrock.com/api/data/reference-data/asset/asset-creation/v1/totalReturnSwapSecurity`（`config/aladdin_endpoint_example.json`） |
| Content-Type | `application/json` |
| 環境 | **未確認**（テスト環境か本番環境か） |
| 認証 | **未提供**（提供されても、コード・Git・ログには書かない） |
| 要求本文 | 利用者から提示された見本の形（`counterparty`、`swapInfo.payLeg／recLeg`、`*PaymentReset`、`*Stub` など） |

- 原文の値をそのまま使える項目（開始日・終了日・通貨・ルックバック日数）だけ入れる
- Aladdin のコード・列挙値・スプレッドの単位・自社の向き（payLeg／recLeg）は、対応表（`config/aladdin_map_example.json` の形。
  埋めたものは `tmp/aladdin_map.json` などに置き、`CURIE_ALADDIN_MAP` で指定）にあるときだけ入れる。なければ未確定として理由を残す
- スプレッドは、条件の値（bp）を対応表の `spread_unit`（bp・decimal・percent）に換算する。例：原文「0.90%」→ 90 bp → decimal 0.009
- 想定元本は、提示された見本に項目がないため、格納先を推測せず未確定として残す
- 送信処理は作っていない（環境の確認と認証情報の提供の後に作る）

## ファイル

| ファイル | 役割 |
|---|---|
| `workflows_c1_case.R` | 1件の全工程（`workflows_c1_case()`）、コンソール表示用の要約 |
| `acquire_document.R` | 取得と記録（curl。User-Agent、形式の判定、追跡用パラメータの除去） |
| `reading_data.R` | 本文の抽出（PDF・HTML の文字コードと &#147; など）。文字のないページは「本文なし」と記録 |
| `claude_agent.R` | Claude Code の作業フォルダ・依頼文・起動（processx）・結果の形式チェック |
| `read_results.R` | 案件フォルダの読込 |
| `validate_results.R` | 条件・根拠・版・FpML の検証 |
| `trs_fields.R` | 抽出項目の定義、値の状態 |
| `verify_quotes.R` | 引用照合のための正規化、SHA-256 |
| `fpml_trs.R` | FpML の作成（デモ用）と、FpML のコード変換表（照合でも使う） |
| `fpml_validate.R` | FpML の構文・スキーマ検証（xml2） |
| `aladdin_payload.R` | Aladdin 送信用の要求本文の下書き（送信はしない） |

## 未完了

- FpML の公式XSD での検証、ISDA 定義の名称と FpML のコード（contractualDefinitions 等）の確認
- Aladdin への送信（送信先がテスト環境か本番環境かの確認、認証情報、列挙値の一覧、Aladdin のコード対応、スプレッドの単位、想定元本の格納先の提示後）
- 同じ資料を複数回読ませたときの揺れの測定、正解つき試験資料での正答率の測定
