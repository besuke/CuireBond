# c1 Copilot 一次読解：Microsoft 365 Copilot Chat API（Microsoft Graph の beta／プレビュー）。
#
# 【注意】/beta の API は予告なく変わることがあり、Microsoft は本番アプリでの利用をサポートしていない。
#         GitHub Copilot とは別製品。利用者に Microsoft 365 Copilot アドオンライセンスが必要。
#   会話作成：POST https://graph.microsoft.com/beta/copilot/conversations （本文は空のJSON、成功は 201）
#   読解依頼：POST https://graph.microsoft.com/beta/copilot/conversations/{id}/chat （成功は 200）
#   https://learn.microsoft.com/en-us/microsoft-365/copilot/extensibility/api/ai-services/chat/overview
#
# モード（黙って切り替えない）
#   api    ：Copilot を実際に呼ぶ
#   manual ：人が Copilot で読ませた回答（抽出JSON、または回答の全文）を取り込む
#   demo   ：モックの抽出JSONを使う。Copilot は呼ばない（成果物に demo と記録する）

. (Join-Path $PSScriptRoot 'common.ps1')

$script:CopilotBetaNote = 'Microsoft 365 Copilot Chat API は beta（プレビュー）。本番利用は Microsoft のサポート対象外'

# Copilot への依頼文（抽出指示書と同じ内容。Claude の結果は含めない）
function New-C1CopilotPrompt($Document, [string]$FileUrl, [string]$InstructionPath) {
  @(
    'あなたは CURIE c1 のTRSターム条件の一次読解担当です。添付の資料だけを根拠にし、推測で補わないでください。'
    'ウェブ検索や他の資料は使わないでください。'
    "対象の資料：$FileUrl（$($Document.file_name)、$($Document.pages)ページ）"
    ''
    '下の抽出指示に従い、回答は抽出JSONを1つだけ ```json で囲んで返してください。説明文は不要です。'
    "document は document_id=$($Document.document_id)、file=$($Document.file_name)、pages=$($Document.pages)、extracted_by=copilot、instruction_version=c1-extract-v1 としてください。"
    '同じ項目が複数ある場合（複数の参照資産、日程の各回など）は、項目ごとに1行とし、seq（1,2,…）と leg_id を付けてください。1つの値にまとめないでください。'
    '資料を読めなかった場合は、JSONを作らず、回答の先頭に「読めなかった」と書いて理由を書いてください。'
    ''
    '----- 抽出指示（instructions.md）-----'
    (Get-Content -LiteralPath $InstructionPath -Raw -Encoding utf8)
  ) -join "`n"
}

# 回答の全文 → 抽出JSON（形式チェック付き）。結果の state：ok／unreadable／invalid
function ConvertFrom-C1CopilotReply([string]$Text) {
  if (-not $Text -or -not $Text.Trim()) { return @{ state = 'invalid'; issues = @('回答が空') } }
  if ($Text.TrimStart().StartsWith('読めなかった')) { return @{ state = 'unreadable'; issues = @($Text.Trim()) } }
  $json = Get-C1JsonFromText $Text
  if (-not $json) { return @{ state = 'invalid'; issues = @('回答にJSONがない') } }
  try { $obj = $json | ConvertFrom-Json -Depth 30 } catch { return @{ state = 'invalid'; issues = @("JSONとして読めない: $($_.Exception.Message)") } }
  $issues = @(Test-C1ExtractionShape $obj)
  return @{ state = if ($issues.Count) { 'invalid' } else { 'ok' }; extraction = $obj; issues = $issues }
}

# Copilot の一次読解を実行し、回答・根拠・会話IDを保存する
# 戻り値：state（ok／access_failed／invalid）、extraction、記録
function Invoke-C1Copilot {
  param([string]$Mode, $Document, $Version, [string]$Token, [string]$AgentDir, [string]$InstructionPath,
        [string]$ManualPath, [string]$DemoPath, [int]$TimeoutSec = 300)
  New-Item -ItemType Directory -Force -Path $AgentDir | Out-Null
  $log = [ordered]@{ mode = $Mode; note = $script:CopilotBetaNote; started_at = (Get-C1Now) }
  $result = @{ state = 'access_failed'; log = $log }
  try {
    Write-C1Log 'Copilot 依頼文作成' "モード $Mode"
    $prompt = New-C1CopilotPrompt $Document $Version.web_url $InstructionPath
    Write-C1Text (Join-Path $AgentDir 'prompt_for_copilot.md') $prompt
    $log.prompt_sha256 = (Get-FileHash -InputStream ([IO.MemoryStream]::new([Text.Encoding]::UTF8.GetBytes($prompt))) -Algorithm SHA256).Hash.ToLower()
    $text = $null
    switch ($Mode) {
      'api' {
        Write-C1Log 'Copilot API実行' '会話を作成しています（beta）'
        $conv = Invoke-C1Graph POST 'beta/copilot/conversations' -Body @{} -Token $Token -Expect 201
        $log.conversation_id = $conv.id
        Write-C1Log 'Copilot API実行' "読解を依頼しています（会話 $($conv.id)、ウェブ検索なし、資料 $($Version.web_url)）"
        $body = @{
          message             = @{ text = $prompt }
          locationHint        = @{ timeZone = 'Asia/Tokyo' }
          contextualResources = @{ files = @(@{ uri = $Version.web_url }); webContext = @{ isWebEnabled = $false } }
        }
        $t0 = Get-Date
        $resp = Invoke-C1Graph POST "beta/copilot/conversations/$($conv.id)/chat" -Body $body -Token $Token -TimeoutSec $TimeoutSec
        $log.chat_seconds = [int]((Get-Date) - $t0).TotalSeconds
        Write-C1Log 'Copilot 回答取得' "$($log.chat_seconds) 秒"
        Write-C1Json (Join-Path $AgentDir 'copilot_response.json') $resp
        # 最後のメッセージが Copilot の回答（先頭は依頼文の写し）
        $reply = @($resp.messages) | Where-Object { $_.text -ne $prompt } | Select-Object -Last 1
        $text = $reply.text
        $log.reply = [ordered]@{ message_id = $reply.id; created = $reply.createdDateTime; sensitivity_label = $reply.sensitivityLabel.displayName }
        $log.attributions = @(@($reply.attributions) | ForEach-Object {
          [ordered]@{ type = $_.attributionType; source = $_.attributionSource; name = $_.providerDisplayName; url = $_.seeMoreWebUrl } })
      }
      'manual' {
        if (-not $ManualPath -or -not (Test-Path $ManualPath)) { throw "manual モードの回答ファイルがありません: $ManualPath" }
        Write-C1Log 'Copilot 回答取得' "手動の回答ファイルを取り込みます：$ManualPath"
        $text = Get-Content -LiteralPath $ManualPath -Raw -Encoding utf8
      }
      'demo' {
        if (-not $DemoPath -or -not (Test-Path $DemoPath)) { throw "demo モードのモック抽出JSONがありません: $DemoPath" }
        Write-C1Log 'Copilot API実行' 'デモ（モック。Copilot は呼ばない）'
        $text = "``````json`n" + (Get-Content -LiteralPath $DemoPath -Raw -Encoding utf8) + "`n``````"
        Write-C1Log 'Copilot 回答取得' "モックの回答：$DemoPath"
      }
      default { throw "Copilot のモードが不正です: $Mode（api／manual／demo）" }
    }
    Write-C1Text (Join-Path $AgentDir 'copilot_reply.md') ($text ?? '')
    $parsed = ConvertFrom-C1CopilotReply $text
    $log.reply_state = $parsed.state
    $log.issues = @($parsed.issues)
    if ($parsed.state -eq 'ok') {
      $x = $parsed.extraction
      # document は PowerShell が確かめた値で上書きする（Copilot の申告ではなく、読ませた版の情報）
      $doc = [ordered]@{ document_id = $Document.document_id; file = $Document.file_name; pages = $Document.pages
                         pdf_sha256 = $Version.sha256; extracted_by = 'copilot'; instruction_version = 'c1-extract-v1'
                         model = 'Microsoft 365 Copilot Chat API (beta)' + $(if ($Mode -eq 'demo') { ' [demo mock]' } elseif ($Mode -eq 'manual') { ' [manual]' } else { '' })
                         copilot_mode = $Mode; conversation_id = $log["conversation_id"]; extracted_at = (Get-C1Now) }
      $x | Add-Member -NotePropertyName document -NotePropertyValue $doc -Force
      Write-C1Json (Join-Path $AgentDir 'copilot_extraction.json') $x
      $result.state = 'ok'; $result.extraction = $x
    } elseif ($parsed.state -eq 'unreadable') {
      $result.state = 'access_failed'; $log.error = 'Copilot が資料を読めなかったと回答'
    } else {
      $result.state = 'invalid'; $log.error = '回答の形式が不正: ' + ($parsed.issues -join '; ')
    }
  } catch {
    $log.error = $_.Exception.Message
    $result.state = 'access_failed'
  }
  $log.finished_at = (Get-C1Now)
  $log.state = $result.state
  Write-C1Json (Join-Path $AgentDir 'copilot_log.json') $log
  if ($log.Contains('error')) { Write-C1Log 'Copilot' $log.error 'error' }
  return $result
}
