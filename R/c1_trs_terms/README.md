# c1_trs_terms：TRSターム条件PDF → 独立読解 → 照合 → 確認表・FpML［デモ実装済み・本番連携は未完了］

各社のTRS確認書・ターム条件PDFを、Claude と Microsoft 365 Copilot に独立して読ませ、
R で照合して確認表（Excel）とFpMLを作る。TRSの価格計算はしない。Aladdin用JSONは仕様の提示後に追加する。

```
読むべきURLをコンソールに貼る（または引数でURL・ファイルを渡す）
          │
          ├─▶ R：同じURLを取得して取得記録（取得元・日時・SHA-256）。照合のためだけに使い、AIには渡さない
          │
          ┌───────────────┴───────────────┐
          ▼                               ▼
   Claude Code（claude -p）          Microsoft 365 Copilot（依頼文 → 出力を取り込み）
   URLを自分で読む（そのドメインだけ   同じURLを自分で読む
   ウェブ取得を許可）
          │  extraction.json／fpml_proposal.xml／review_items.md（AIごとに保存）
          │  読めなかったら「読めなかった」と理由だけを書いて終える（別の手段で読ませない）
          └───────────────┬───────────────┘
                          ▼
     R：引用の実在確認 → AI間の照合 → 規則チェック → 人の確認の反映 → FpMLの構文・スキーマ検証
                          ▼
     最終成果物（確認表Excel・FpML）を保存 → 成果物の保存先へ保存 → 通知（完了／要確認／失敗と保存先）
```

入力PDFは SharePoint に上げない（既定 `upload_input = "never"`）。必要なら `"auto"`（ローカルファイルだけ）・`"always"` に切り替える。

## 守ること

- **推測で埋めない**：値の状態（記載あり／空欄／選択肢（未確定）／記載なし）を区別する
- **確定の条件**：全AIの値が一致し、引用がPDFの指定ページに実在する項目、または人が確認した項目
- **確認待ち**：AI間の不一致、必須項目の不足、AIの出力がそろわない場合は「要確認」
- **区別すること**：参照資産・指数と資金調達の参照金利、参照資産の評価日と資金調達金利のフィキシング／リセット日
- **旧参照金利**：原文どおり（例：USD-LIBOR-BBA を SOFR に置き換えない）
- **AIが自分で読む**：AIには元のURL（またはファイル）を渡す。R が本文を抜き出してAIに渡すことはしない。
  読めなかったAIは「読めなかった」と記録して終え、その分は照合で「片方のみ」「未取得」となり確認待ちになる
- **版の確認**：R が取得した版の SHA-256・ページ数・取得日時を依頼文に書く。URLの中身は公開元が差し替える可能性があるため、
  AIが読んだ版のページ数が違えば要確認事項に書かせ、引用の実在確認で食い違いを検出する
- **再現性**：同じ抽出JSONからは毎回同じ確認表・FpMLを作る（FpMLのSHA-256を記録）。AIの読取のぶれは、照合と引用の実在確認で検出する
- **AIの出力と最終成果物を分ける**：`agents/claude`・`agents/copilot` と `final` は別フォルダ

## 実行方法

```r
# PDF 1件。引数なしなら、読むべきURLをコンソールに貼る。成果物の保存先・通知はデモ
Rscript scripts/run_c1_document.R
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

## 現行の方式：Copilot の一次読解 → Claude の原文照合・FpML 作成（PowerShell）→ R の検証

独立した二重読解ではない（Copilot の一次読解を Claude が原文で確かめる）。「2AI独立照合済み」とは表示しない。

| 担当 | 処理 |
|---|---|
| PowerShell（`PS/`） | 委任認証、共有URLの資料の取得と版の記録、Copilot 一次読解、Claude 原文照合・FpML 作成、CSV・JSON・FpML の保存 |
| R | 終了コード・成果物の受取、条件・根拠・版・FpML（XML構文／公式XSD／CSV との一致）の検証、tibble・Excel 表示 |

```r
source("scripts/run_c1_document.R", encoding = "UTF-8")   # 「配置済み資料の共有URLを貼ってください:」
```

- 資料は利用者が事前に SharePoint へ配置する。R・PowerShell はアップロードしない。両方の読解に同じ資料（同じ版。SHA-256・eTag で確認）を使う
- モード：Copilot `api`（Chat API。beta／プレビュー）・`manual`（手動回答）・`demo`（モック）、Claude `api`・`demo`。黙って切り替えない
- 接続確認（契約書の前に）：`pwsh -File R/c1_trs_terms/PS/test_connection.ps1`（サインイン → Copilot への短い質問）
- 接続情報（環境変数。コード・Git に書かない）：`CURIE_M365_TENANT_ID`、`CURIE_M365_CLIENT_ID`
- 必要なもの：Microsoft 365 Copilot アドオンライセンス、アプリ登録（公開クライアントフロー許可）、委任権限 7つ
  （Sites.Read.All, Mail.Read, People.Read.All, OnlineMeetingTranscript.Read.All, Chat.Read, ChannelMessage.Read.All,
  ExternalItem.Read.All。共有リンク形式のURLは Files.ReadWrite も）と管理者同意
- 認証は Invoke-RestMethod によるデバイスコード方式（Microsoft.Graph.Authentication は Smart App Control に DLL を止められたため使わない）。トークンはメモリだけ
- Claude が PDF を読むには poppler（pdftoppm）が必要（winget の oschwartz10612.Poppler）
- 成果物：`output/c1_trs/cases/<案件ID>/`（manifest.json、result/conditions.csv・conditions.json・fpml.xml・review_items.md・evidence.json・c1_validation.xlsx、r_validation.json）
- 終了コード：0 成果物あり、2 未接続、3 資料アクセス失敗、4 Copilot 読解失敗、5 Copilot 回答形式不正、6 Claude 読解失敗、7 Claude 回答形式不正
- モックテスト：`Rscript scripts/test_c1_case_mock.R`
- Aladdin 用JSONは仕様の提示前のため作らない
- 旧方式（独立した二重読解）：`scripts/run_c1_document_independent.R`
