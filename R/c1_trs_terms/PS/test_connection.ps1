<#
c1：Microsoft 365 Copilot への接続確認（契約書を読ませる前に行う）。
  1. Microsoft へのサインイン（委任認証。デバイスコード方式）
  2. Copilot Chat API（beta）への短い質問と、実際の回答の取得（ウェブ検索なし、資料なし）
トークンは表示・保存しない。結果は output/c1_trs/connection_tests/ に保存する。
終了コード：0 成功、2 未接続（接続情報の不足・サインイン失敗）、4 Copilot の呼び出し失敗

  pwsh -File R/c1_trs_terms/PS/test_connection.ps1
#>
param([string]$OutputRoot)

. (Join-Path $PSScriptRoot 'common.ps1')
$repo = (Resolve-Path (Join-Path $PSScriptRoot '..\..\..')).Path
if (-not $OutputRoot) { $OutputRoot = Join-Path $repo 'output\c1_trs\connection_tests' }
$dir = Join-Path $OutputRoot ('test_' + (Get-Date -Format 'yyyyMMdd_HHmmss'))
New-Item -ItemType Directory -Force -Path $dir | Out-Null
$script:C1LogPath = Join-Path $dir 'run_log.jsonl'
$rec = [ordered]@{ started_at = (Get-C1Now); note = 'Microsoft 365 Copilot Chat API は beta（プレビュー）'
                   sign_in = '未実行'; copilot_question = '未実行' }
$exit = 0
try {
  $missing = Test-C1Connection
  if ($missing.Count) {
    Write-C1Log '接続情報' ('不足：' + ($missing -join ', ')) 'error'
    Write-Host @'
次を設定してください（値は会社の管理者から入手。コード・Git には書かない）：
  CURIE_M365_TENANT_ID   … Microsoft Entra のテナントID
  CURIE_M365_CLIENT_ID   … アプリ登録のクライアントID（公開クライアントフローを許可）
アプリ登録に必要な委任権限（管理者同意）：
  Sites.Read.All, Mail.Read, People.Read.All, OnlineMeetingTranscript.Read.All,
  Chat.Read, ChannelMessage.Read.All, ExternalItem.Read.All（共有リンクを使う場合は Files.ReadWrite も）
利用者に Microsoft 365 Copilot アドオンライセンスが必要です。
'@
    Stop-C1 'not_connected' ('接続情報がありません: ' + ($missing -join ', '))
  }
  Write-C1Log '1. サインイン' '委任認証（デバイスコード）を開始します'
  $token = Get-C1GraphToken $script:CopilotScopes
  $rec.sign_in = '成功'
  Write-C1Log '1. サインイン' '成功'
  try {
    Write-C1Log '2. Copilot 質問' '会話を作成しています（beta）'
    $conv = Invoke-C1Graph POST 'beta/copilot/conversations' -Body @{} -Token $token -Expect 201
    $question = '接続確認です。「接続できました」とだけ日本語で答えてください。'
    $resp = Invoke-C1Graph POST "beta/copilot/conversations/$($conv.id)/chat" -Token $token -TimeoutSec 120 -Body @{
      message = @{ text = $question }; locationHint = @{ timeZone = 'Asia/Tokyo' }
      contextualResources = @{ webContext = @{ isWebEnabled = $false } } }
    $reply = @($resp.messages) | Where-Object { $_.text -ne $question } | Select-Object -Last 1
    $rec.conversation_id = $conv.id
    $rec.reply = $reply.text
    $rec.copilot_question = if ($reply.text) { '成功（回答あり）' } else { '応答はあったが回答が空' }
    Write-C1Log '2. Copilot 質問' ("回答：" + $reply.text)
    if (-not $reply.text) { $exit = 4 }
  } catch {
    $rec.copilot_question = '失敗'; $rec.error = $_.Exception.Message; $exit = 4
    Write-C1Log '2. Copilot 質問' $_.Exception.Message 'error'
  }
  $token = $null
} catch [C1Failure] {
  $rec.sign_in = '失敗（未接続）'; $rec.error = $_.Exception.Message; $exit = 2
}
$rec.finished_at = (Get-C1Now); $rec.exit_code = $exit
Write-C1Json (Join-Path $dir 'connection_test.json') $rec
Write-Output "C1_TEST_DIR=$dir"
exit $exit
