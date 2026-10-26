# c1 Claude による原文の読解と FpML 作成。
#
# Claude Code を非対話モード（claude -p）で起動し、作業フォルダの次のファイルを渡す。
#   document.pdf／document.htm … 取得した原資料（SHA-256 を記録済み。HTML は HTML のまま）
#   instructions.md         … 抽出指示書（docs/c1_trs_extraction_instructions.md）
# Claude は原文から全項目を読み取り、値・根拠（ページ・位置・引用）・未確定事項を記録する。推測で補わない。
# 作るファイル：reviewed.json（条件）、fpml.xml（FpML 案）、review_items.md（要確認事項）
# 使えるツールは Read と Write だけ（ウェブ取得なし。読み書きは作業フォルダ内）。PDF を読むのに poppler が必要。
#
# モード：api（Claude Code を実行）／demo（用意した結果ファイルを使う。Claude は呼ばない）

. (Join-Path $PSScriptRoot 'common.ps1')

$script:ClaudeOutputs = @('reviewed.json', 'fpml.xml', 'review_items.md')

function New-C1ClaudePrompt($Document) {
  @(
    '# CURIE c1：TRS 確認書の読解と FpML 作成'
    ''
    "このフォルダの ``$($Document.local_name)``（原文）と ``instructions.md``（抽出指示）を読んでください。"
    'すべての項目を原文から読み取り、値・根拠ページ・原文の引用を記録すること。推測で補わないこと。'
    'page は PDF のページ番号。HTML の資料は全体を1ページ（page=1）とし、location に条項名・見出しを書くこと。'
    ''
    '## 作るファイル（このフォルダに保存）'
    "1. ``reviewed.json``：instructions.md の抽出JSONの形式に、項目ごとに次を加える。document は document_id=$($Document.document_id)、file=$($Document.file_name)、pages=$($Document.pages)、extracted_by=claude"
    '   - `leg_id`：trade／return_leg／interest_leg など。`seq`：同じ項目が複数あるときの番号（1から）。1つの値にまとめない'
    '   - `unit`：値の単位（日付は date、スプレッドは bp、下限は percent、日数は business_days、金額は通貨コード、単位がなければ空）'
    '   - `location`：ページ内の位置（条項名・見出しなど。分かる範囲）'
    '   - `review_status`：confirmed（原文で確認した値）／unresolved（空欄・選択肢・解釈不明で未確定）／not_found（記載なし）'
    '   - `review_note`：未確定の理由、解釈に迷った点'
    '   - instructions.md のすべての項目を出すこと'
    '2. `fpml.xml`：reviewed.json の value_status=stated の値だけで作る FpML 5.x confirmation の returnSwap（returnLeg と interestLeg）。'
    '   未確定・記載なしの項目は要素を作らず、XMLコメントに列挙する。推測で埋めない'
    '3. `review_items.md`：要確認事項（未確定事項、解釈に迷った点）と根拠ページの一覧'
    ''
    '## 読めなかった場合'
    '原文を読めなかった場合は、別の手段で読もうとせず、reviewed.json と fpml.xml は作らないこと。'
    'review_items.md の先頭に「読めなかった」と書き、理由を書くこと。'
    'このフォルダ以外のファイルを読み書きしないこと。'
  ) -join "`n"
}

function Invoke-C1Claude {
  param([string]$Mode, $Document, [string]$SourcePath,
        [string]$AgentDir, [string]$InstructionPath, [string]$DemoDir, [string]$ClaudeCmd = 'claude', [int]$TimeoutSec = 1800)
  New-Item -ItemType Directory -Force -Path $AgentDir | Out-Null
  $log = [ordered]@{ mode = $Mode; started_at = (Get-C1Now) }
  $result = @{ state = 'access_failed'; log = $log }
  try {
    Write-C1Log 'Claude 依頼文作成' "モード $Mode"
    Copy-Item -LiteralPath $SourcePath -Destination (Join-Path $AgentDir $Document.local_name) -Force
    Copy-Item -LiteralPath $InstructionPath -Destination (Join-Path $AgentDir 'instructions.md') -Force
    Write-C1Text (Join-Path $AgentDir 'prompt.md') (New-C1ClaudePrompt $Document)
    if ($Mode -eq 'api') {
      # PDF を読むための poppler を PATH に加える（Claude Code の Read が pdftoppm を使う）
      if ($Document.kind -eq 'pdf' -and -not (Add-C1PopplerPath)) { throw 'pdftoppm（poppler）がないため、Claude が PDF を読めません' }
      Write-C1Log 'Claude 実行' '原文の読解と FpML 作成（数分かかります。この間は表示が止まります）'
      $prompt = "Read prompt.md in the current folder and follow it exactly. Read $($Document.local_name) yourself; if you cannot read it, write only review_items.md explaining why. Otherwise create reviewed.json, fpml.xml and review_items.md in this folder."
      $t0 = Get-Date
      Push-Location $AgentDir
      try {
        $out = & $ClaudeCmd -p $prompt --output-format json --allowedTools Read Write --permission-mode acceptEdits 2>&1
        $log.exit_status = $LASTEXITCODE
      } finally { Pop-Location }
      $log.seconds = [int]((Get-Date) - $t0).TotalSeconds
      Write-C1Text (Join-Path $AgentDir 'claude_run_log.json') (($out | Out-String))
      try {
        $run = ($out | Out-String) | ConvertFrom-Json
        $log.cost_usd = $run.total_cost_usd; $log.num_turns = $run.num_turns; $log.is_error = $run.is_error
      } catch { }
      $log.cli_version = (& $ClaudeCmd --version 2>$null | Select-Object -First 1)
      Write-C1Log 'Claude 回答取得' ("{0} 秒、費用の目安 {1} USD" -f $log.seconds, $log.cost_usd)
    } elseif ($Mode -eq 'demo') {
      if (-not $DemoDir -or -not (Test-Path $DemoDir)) { throw "Claude の demo モードの結果フォルダがありません: $DemoDir" }
      Write-C1Log 'Claude 実行' 'デモ（用意した結果を使う。Claude は呼ばない）'
      $script:ClaudeOutputs | Where-Object { Test-Path (Join-Path $DemoDir $_) } |
        ForEach-Object { Copy-Item -LiteralPath (Join-Path $DemoDir $_) -Destination (Join-Path $AgentDir $_) -Force }
      Write-C1Log 'Claude 回答取得' "デモの結果：$DemoDir"
    } else { throw "Claude のモードが不正です: $Mode（api／demo）" }

    $review = Join-Path $AgentDir 'review_items.md'
    $reviewed = Join-Path $AgentDir 'reviewed.json'
    if (-not (Test-Path $reviewed)) {
      $head = if (Test-Path $review) { (Get-Content -LiteralPath $review -TotalCount 5 -Encoding utf8) -join ' ' } else { '' }
      $log.error = if ($head -match '読めなかった') { 'Claude が原文を読めなかったと報告（review_items.md）' } else { 'Claude の結果（reviewed.json）がない' }
      $result.state = 'access_failed'
    } else {
      $obj = $null
      try { $obj = Get-Content -LiteralPath $reviewed -Raw -Encoding utf8 | ConvertFrom-Json -Depth 30 } catch { }
      $issues = @(Test-C1ExtractionShape $obj @('review_status'))
      if (-not (Test-Path (Join-Path $AgentDir 'fpml.xml'))) { $issues += 'fpml.xml がない' }
      $log.issues = $issues
      if ($issues.Count) { $result.state = 'invalid'; $log.error = '回答の形式が不正: ' + ($issues -join '; ') }
      else { $result.state = 'ok'; $result.reviewed = $obj }
    }
  } catch {
    $log.error = $_.Exception.Message
    $result.state = 'access_failed'
  }
  $log.finished_at = (Get-C1Now); $log.state = $result.state
  Write-C1Json (Join-Path $AgentDir 'claude_log.json') $log
  if ($log.Contains('error')) { Write-C1Log 'Claude' $log.error 'error' }
  return $result
}
