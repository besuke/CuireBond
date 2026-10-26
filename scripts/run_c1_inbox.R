# c1：フォルダー投入型の処理（RStudio から実行する例）。CURIE OTC Derivative
# 監視フォルダーの直下に PDF を置くと、案件ごとに processed/<共通名>/ を作り、PDF と成果物を保存する。
# Curieプロジェクトの最上位フォルダで source("R/load_Curie.R") したあとは、作業ディレクトリに依存しない。
#
# 監視フォルダー：環境変数 Curie_C1_INBOX_ROOT（tmp/.Renviron。いまは C:/AnalyticFin/temp/TRS_curie_PDF）
#   <監視フォルダー>/*.pdf            処理する PDF（直下だけ。processed などのサブフォルダーは監視しない）
#   processed/<共通名>/              共通名.pdf・共通名.tsv・共通名.xml（FpML）・共通名_payload.json（Aladdin 用。送信しない）・validation.json
#   review/<共通名>/                 要確認（取引日・取引最終日・相手先のどれかが不明。不明な部分は UNKNOWN）
#   failed/                          失敗の理由（*.reason.json）。元の PDF は監視フォルダーに残る（次からは保留）
#   duplicates/                      処理済みと同じ内容の PDF（AI は呼ばない）
#   history.jsonl                    処理履歴（元のファイル名と保存後の名前の対応）
# 共通名：TRS_取引日_取引最終日_相手先の先頭5文字（日付は YYYYMMDD）。元の PDF は、出力の保存が終わってから監視フォルダーから消す。
# 相手先を決めるには、自社の名前を対応表（Curie_ALADDIN_MAP の our_party_names）に入れておく（なければ要確認になる）。

source("R/load_Curie.R", encoding = "UTF-8")

# 1. 設定（root を省くと Curie_C1_INBOX_ROOT を使う）
config <- c1_inbox_config(stable_sec = 10)          # サイズと更新時刻が 10 秒変わらなければコピー完了とみなす

# 2. 1回スキャンして処理する（起動メッセージと監視フォルダーを表示し、直下の PDF を purrr::walk() で1件ずつ処理）
scan <- scan_c1_inbox_once(config, claude_mode = "api")
print(scan)

# 3. 処理履歴（元のファイル名・状態・保存後の名前・理由）
read_c1_history(config) |> dplyr::mutate(status = .status_en_c1(status)) |>
  dplyr::select(time, original_name, status, saved_name, reason) |> print(n = Inf, width = Inf)

# 4. 定期監視（同じ scan_c1_inbox_once() を 60 秒ごとに呼ぶ。別の R プロセスで動くので、RStudio のコンソールはそのまま使える）
# start_c1_inbox_watch(config, interval_sec = 60, claude_mode = "api")
# c1_inbox_watch_status(config)                    # 状態と最後のスキャン時刻、ログ（logs/watch.log）の場所
# stop_c1_inbox_watch(config)                      # 処理中の1件が終わってから止まる（force = TRUE ですぐに止める）

# 前回失敗した PDF を処理し直す：scan_c1_inbox_once(config, claude_mode = "api", retry_failed = TRUE)

# 自社の名前を設定した後に、要確認（review/）の案件を作り直す（保存済みの抽出結果 extraction/ を使い、AI は呼ばない）
#   自社の名前：tmp/.Renviron に Curie_OUR_PARTY_NAMES=自社の名前（複数は ; 区切り）。R を再起動するか readRenviron("tmp/.Renviron")
# regenerate_c1_review_cases(config)
# regenerate_c1_case("C:/AnalyticFin/temp/TRS_curie_PDF/review/<案件フォルダー>", config)   # 1件だけ
# 将来の並列化（今は逐次が標準）：future::plan(future::multisession); scan_c1_inbox_once(config, .walk = furrr::future_walk)
