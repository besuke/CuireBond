# c1_trs_terms：TRSターム条件PDF → 独立読解 → 照合 → 確認表・FpML［デモ実装済み・本番連携は未完了］

各社のTRS確認書・ターム条件PDFを、Claude と Microsoft 365 Copilot に独立して読ませ、
R で照合して確認表（Excel）とFpMLを作る。TRSの価格計算はしない。Aladdin用JSONは仕様の提示後に追加する。

```
URL／ローカルファイル ─▶ R：取得（ダウンロード or 複製）・取得記録（取得元・日時・SHA-256）
                          │
                          ▼
                   ローカルファイル：SharePoint に配置（保存先URL・識別情報）→ 同じ版を取り直してSHA-256を確認
                   URL：SharePoint には上げず、元のURLを共有先にする（upload_input = "always" で上げる）
                          │
          ┌───────────────┴───────────────┐
          ▼                               ▼
   Claude Code（claude -p）          Microsoft 365 Copilot（依頼文 → 出力を取り込み）
   同じ版のPDFを作業フォルダで読む     SharePoint のファイルを読む
          │  extraction.json／fpml_proposal.xml／review_items.md（AIごとに保存）
          └───────────────┬───────────────┘
                          ▼
     R：引用の実在確認 → AI間の照合 → 規則チェック → 人の確認の反映 → FpMLの構文・スキーマ検証
                          ▼
     最終成果物（確認表Excel・FpML）を保存 → SharePoint に保存 → 通知（完了／要確認／失敗と保存先）
```

## 守ること

- **推測で埋めない**：値の状態（記載あり／空欄／選択肢（未確定）／記載なし）を区別する
- **確定の条件**：全AIの値が一致し、引用がPDFの指定ページに実在する項目、または人が確認した項目
- **確認待ち**：AI間の不一致、必須項目の不足、AIの出力がそろわない場合は「要確認」
- **区別すること**：参照資産・指数と資金調達の参照金利、参照資産の評価日と資金調達金利のフィキシング／リセット日
- **旧参照金利**：原文どおり（例：USD-LIBOR-BBA を SOFR に置き換えない）
- **同じ版を読む**：SharePoint のURLを渡すだけでAIが読めるとは仮定しない。Claude には、取り直して SHA-256 を確かめたPDFを作業フォルダで渡す
- **再現性**：同じ抽出JSONからは毎回同じ確認表・FpMLを作る（FpMLのSHA-256を記録）。AIの読取のぶれは、照合と引用の実在確認で検出する
- **AIの出力と最終成果物を分ける**：`agents/claude`・`agents/copilot` と `final` は別フォルダ

## 実行方法

```r
# PDF 1件（URL またはローカルファイル）。SharePoint・通知はデモ
Rscript scripts/run_c1_document.R https://www.sc.com/en/uploads/sites/66/content/docs/Total-Return-Swap.pdf
Rscript scripts/run_c1_document.R C:/path/to/termsheet.pdf --copilot-dir=C:/path/to/copilot_outputs
Rscript scripts/run_c1_document.R <URL> --no-claude      # Claude を起動しない（流れの確認）

# デモ（架空PDF・手作りの2AI抽出）：照合 → 人の確認 → 正式FpML → 再現性・引用改ざんの検出
source("scripts/run_demo_c1_TRS.R", encoding = "UTF-8")
```

R から呼ぶ場合：`workflows_trs_document_c1(source, sharepoint = sharepoint_settings_c1(...), run_claude = TRUE, copilot_dir = NULL, overrides_path = NULL, notifier = NULL)`

## 実行ごとのフォルダ（`output/c1_trs/runs/<文書>_<日時>/`）

| フォルダ・ファイル | 内容 |
|---|---|
| `intake/` | 取得したPDFと取得記録（`acquisition_manifest.csv`） |
| `agents/claude/` | Claude への依頼文、同じ版のPDF、Claude の出力（JSON・FpML案・要確認事項）、R が作った条件一覧Excelと検証用FpML、実行ログ |
| `agents/copilot/` | Copilot への依頼文（`prompt_for_copilot.md`）。Copilot の出力をここに置く（または `--copilot-dir`） |
| `final/` | 照合後の最終成果物：確認表Excel、FpML（正式版またはドラフト）、FpML検証結果 |
| `run_summary.json` | 状態（完了／要確認／失敗）、入力のSHA-256・SharePoint保存先、AIの出力の状態、理由 |

## 設定と接続（未接続の部分はデモ）

| 部分 | 現在 | 本番 |
|---|---|---|
| SharePoint | 入力がURLなら上げない（元のURLを共有先にする）。ローカルファイルと成果物は、デモ：`output/sharepoint_demo/` をSharePointに見立てる | `sharepoint_settings_c1(mode = "microsoft365r", site_url =, library =, folder =)`。Microsoft365R で接続（認証は Microsoft365R に任せ、コードに書かない）。接続情報の提示後に導入・検証 |
| Claude | Claude Code を `claude -p` で実行（Read・Write のみ許可、作業フォルダ内） | 同じ |
| Copilot | 依頼文を作り、出力を取り込む（未取得なら確認待ち） | 実行手段（Copilot の API 等）が決まったら自動化 |
| FpMLのスキーマ検証 | 構文（整形式）のみ。XSD未設定 | 公式XSDを環境変数 `CURIE_FPML_XSD` で指定すると xml2 で検証 |
| 通知 | デモ：コンソールと `output/c1_trs/runs/notifications.jsonl` | Teams・メール等の通知関数に差し替え（`notifier` 引数） |

| ファイル | 役割 |
|---|---|
| `trs_fields.R` | 抽出項目の定義、値の状態 |
| `acquire_document.R` | URL・ローカルファイルからの取得と取得記録 |
| `sharepoint.R` | SharePoint への配置・取得（デモ／Microsoft365R） |
| `agents.R` | Claude・Copilot への依頼、Claude の実行、AIの出力の取り込み |
| `read_extractions.R` | 抽出JSONの読込と形式チェック（jsonlite） |
| `verify_quotes.R` | 引用がPDFに実在するかの確認、PDFのSHA-256照合（pdftools・openssl） |
| `reconcile_terms.R` | AI間の照合、人の確認の反映、規則チェック |
| `fpml_trs.R` | FpMLの作成（xml2） |
| `fpml_validate.R` | FpMLの構文・スキーマ検証（xml2） |
| `notify.R` | 通知（デモ） |
| `workflows_trs_terms.R` | 抽出JSON → 確認表・FpML（`workflows_trs_terms_c1()`） |
| `workflows_trs_document.R` | PDF 1件の全工程（`workflows_trs_document_c1()`） |

## 未完了

- SharePoint の実接続（接続情報の提示後）、Copilot の実行の自動化、通知先の設定
- Aladdin用JSONへの変換（仕様の提示後）
- FpMLの公式XSDでの検証、ISDA定義の名称とFpMLのコード（contractualDefinitions等）の確認
- 同じAIを複数回実行したときの揺れの測定、正解つき試験PDFでの正答率の測定
