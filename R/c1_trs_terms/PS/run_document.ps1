<#
c1：配置済み資料1件の読解（PowerShell の入口。単独でも、R から起動しても動く）。

  流れ：資料の取得と版の記録 → Copilot の一次読解 → Claude の原文照合・FpML 作成 → CSV・JSON・FpML の保存
  成果物：output/c1_trs/cases/<案件ID>/（このスクリプトのフォルダには保存しない）
  終了コード：0 成果物あり（R の検証待ち）、2 未接続、3 資料アクセス失敗、4 Copilot 読解失敗、
              5 Copilot 回答形式不正、6 Claude 読解失敗、7 Claude 回答形式不正、1 想定外のエラー
  これは独立した二重読解ではない（Copilot の一次読解を Claude が原文で確かめる）。「2AI独立照合済み」とは表示しない。

例：
  pwsh -File R/c1_trs_terms/PS/run_document.ps1 -SharedUrl "https://contoso.sharepoint.com/sites/CURIE/Shared Documents/c1/TRS.pdf" -CopilotMode api
  pwsh -File R/c1_trs_terms/PS/run_document.ps1 -SharedUrl data/demo_c1_trs/pdf/DEMO_TRS_confirmation.pdf -CopilotMode demo -CopilotDemoPath <抽出JSON> -ClaudeMode demo -ClaudeDemoDir <フォルダ>
#>
param(
  [Parameter(Mandatory)][string]$SharedUrl,
  [Parameter(Mandatory)][ValidateSet('api', 'manual', 'demo')][string]$CopilotMode,
  [ValidateSet('api', 'demo')][string]$ClaudeMode = 'api',
  [string]$CaseId,
  [string]$OutputRoot,
  [string]$CopilotManualPath,
  [string]$CopilotDemoPath,
  [string]$ClaudeDemoDir,
  [string]$InstructionPath
)

. (Join-Path $PSScriptRoot 'common.ps1')
. (Join-Path $PSScriptRoot 'invoke_copilot.ps1')
. (Join-Path $PSScriptRoot 'invoke_claude.ps1')

$repo = (Resolve-Path (Join-Path $PSScriptRoot '..\..\..')).Path
if (-not $OutputRoot) { $OutputRoot = Join-Path $repo 'output\c1_trs\cases' }
if (-not $InstructionPath) { $InstructionPath = Join-Path $repo 'docs\c1_trs_extraction_instructions.md' }
$SharedUrl = $SharedUrl.Trim().Trim('"')
$isUrl = $SharedUrl -match '^https?://'
$name = if ($isUrl) { [uri]::UnescapeDataString(([uri]$SharedUrl).Segments[-1]) } else { Split-Path -Leaf $SharedUrl }
$stem = ([IO.Path]::GetFileNameWithoutExtension($name) -replace '[^A-Za-z0-9_-]', '_')
if (-not $stem) { $stem = 'document' }
if (-not $CaseId) { $CaseId = $stem + '_' + (Get-Date -Format 'yyyyMMdd_HHmmss') }
$caseDir = Join-Path $OutputRoot $CaseId
foreach ($d in 'source', 'agents', 'result', 'logs') { New-Item -ItemType Directory -Force -Path (Join-Path $caseDir $d) | Out-Null }
$script:C1LogPath = Join-Path $caseDir 'logs\run_log.jsonl'

$manifest = [ordered]@{
  case_id = $CaseId; created_at = (Get-C1Now); shared_url = $SharedUrl
  copilot_mode = $CopilotMode; claude_mode = $ClaudeMode; copilot_note = $script:CopilotBetaNote
  reading = 'Copilot の一次読解を Claude が原文で確かめる（独立した二重読解ではない）'
  stages = [ordered]@{ document = '未実行'; copilot = '未実行'; claude = '未実行'; outputs = '未実行' }
}
$exit = $script:ExitCodes.ok
$token = $null
try {
  # ---- 1. 資料の取得と版の記録 -------------------------------------------------------
  Write-C1Log '資料の取得' $SharedUrl
  $isSharePoint = $isUrl -and (Test-C1SharePointUrl $SharedUrl)
  if ($CopilotMode -eq 'api' -and -not $isSharePoint) {
    Stop-C1 'document_access' 'Copilot（api）は SharePoint／OneDrive 上の資料しか読めません。配置済み資料の共有URLを指定してください'
  }
  if ($isSharePoint) {
    # 必要な権限：api は Copilot の7権限。共有リンクの解決には Files.ReadWrite が最小権限（公式仕様）
    $scopes = if ($CopilotMode -eq 'api') { $script:CopilotScopes } else { @('Sites.Read.All') }
    if (Test-C1SharingLink $SharedUrl) { $scopes += 'Files.ReadWrite' }
    Write-C1Log 'サインイン' ('委任認証（デバイスコード）。権限：' + ($scopes -join ', '))
    $token = Get-C1GraphToken $scopes
  }
  $raw = Join-Path $caseDir 'source\downloaded.bin'
  try {
    if ($isSharePoint) {
      $item = Get-C1DriveItem $SharedUrl $token
      Save-C1DriveItemContent $item $raw
    } elseif ($isUrl) {
      Invoke-WebRequest -Uri $SharedUrl -OutFile $raw -TimeoutSec 300 -UserAgent ($env:CURIE_HTTP_USER_AGENT ?? 'CURIE-c1') | Out-Null
    } else {
      if (-not (Test-Path -LiteralPath $SharedUrl)) { throw "ファイルがありません: $SharedUrl" }
      Copy-Item -LiteralPath $SharedUrl -Destination $raw -Force
    }
  } catch { Stop-C1 'document_access' ("資料を取得できません: " + $_.Exception.Message) }
  $kind = Get-C1DocumentKind $raw
  $pdf = Join-Path $caseDir 'source\document.pdf'
  switch ($kind) {
    'pdf' { Move-Item -LiteralPath $raw -Destination $pdf -Force }
    'html' { $html = Join-Path $caseDir 'source\document.htm'; Move-Item -LiteralPath $raw -Destination $html -Force; Convert-C1HtmlToPdf $html $pdf }
    default { Stop-C1 'document_access' 'PDF でも HTML でもありません' }
  }
  Add-C1PopplerPath | Out-Null
  $pages = $null
  if (Get-Command pdfinfo -ErrorAction SilentlyContinue) {
    $pages = [int](((& pdfinfo $pdf 2>$null) | Where-Object { $_ -match '^Pages:' }) -replace '\D', '')
  }
  $version = if ($isSharePoint) { Get-C1VersionRecord $item $pdf } else {
    [ordered]@{ item_id = $null; name = $name; web_url = $SharedUrl; e_tag = $null; size = (Get-Item $pdf).Length; sha256 = (Get-C1Sha256 $pdf); checked_at = (Get-C1Now) } }
  $document = [ordered]@{ document_id = $stem; file_name = $name; pages = $pages; sha256 = (Get-C1Sha256 $pdf); source_kind = $kind }
  $manifest.document = $document
  $manifest.sharepoint_before = $version
  $manifest.stages.document = '取得済み'
  Write-C1Log '資料の取得' ("{0}（{1}、{2} ページ、SHA-256 {3}…）" -f $name, $kind.ToUpper(), $pages, $document.sha256.Substring(0, 12))

  # ---- 2. Copilot の一次読解 ---------------------------------------------------------
  $cop = Invoke-C1Copilot -Mode $CopilotMode -Document $document -Version $version -Token $token `
    -AgentDir (Join-Path $caseDir 'agents\copilot') -InstructionPath $InstructionPath -ManualPath $CopilotManualPath -DemoPath $CopilotDemoPath
  $manifest.copilot = $cop.log
  $manifest.stages.copilot = @{ ok = '取得済み'; access_failed = '読解失敗'; invalid = '回答形式不正' }[$cop.state]
  if ($CopilotMode -eq 'demo') { $manifest.stages.copilot += '（デモ：Copilot は呼んでいない）' }
  if ($cop.state -eq 'access_failed') { Stop-C1 'copilot_access' $cop.log.error }
  if ($cop.state -eq 'invalid') { Stop-C1 'copilot_format' $cop.log.error }
  # 読解中に資料が差し替わっていないか（eTag を取り直す）
  if ($isSharePoint) {
    $after = Get-C1DriveItem $SharedUrl $token
    $manifest.sharepoint_after = [ordered]@{ e_tag = $after.eTag; size = $after.size; last_modified = $after.lastModifiedDateTime; checked_at = (Get-C1Now) }
    $manifest.sharepoint_unchanged = ($after.eTag -eq $version.e_tag)
  }
  $token = $null   # これ以降 Microsoft は呼ばない

  # ---- 3. Claude の原文照合・FpML 作成 -----------------------------------------------
  $cla = Invoke-C1Claude -Mode $ClaudeMode -Document $document -PdfPath $pdf `
    -CopilotExtractionPath (Join-Path $caseDir 'agents\copilot\copilot_extraction.json') `
    -AgentDir (Join-Path $caseDir 'agents\claude') -InstructionPath $InstructionPath -DemoDir $ClaudeDemoDir
  $manifest.claude = $cla.log
  $manifest.stages.claude = @{ ok = '取得済み'; access_failed = '読解失敗'; invalid = '回答形式不正' }[$cla.state]
  if ($ClaudeMode -eq 'demo') { $manifest.stages.claude += '（デモ：Claude は呼んでいない）' }
  if ($cla.state -eq 'access_failed') { Stop-C1 'claude_access' $cla.log.error }
  if ($cla.state -eq 'invalid') { Stop-C1 'claude_format' $cla.log.error }

  # ---- 4. 成果物：CSV（1項目1行）・JSON・FpML・要確認事項 ----------------------------
  Write-C1Log '成果物の保存' (Join-Path $caseDir 'result')
  $legOf = { param($f) switch -Regex ($f) { '^return\.' { 'return_leg' } '^funding\.' { 'interest_leg' } '^schedule\.' { 'schedule' } default { 'trade' } } }
  $str = { param($v) if ($null -eq $v) { '' } elseif ($v -is [array]) { ($v | ForEach-Object { "$_" }) -join ' / ' } else { "$v" } }
  $rows = @(@($cla.reviewed.fields) | ForEach-Object {
    $f = $_; $p = $f.PSObject.Properties.Name
    [pscustomobject][ordered]@{
      case_id = $CaseId; item_id = $f.field
      leg_id = $(if ($p -contains 'leg_id' -and $f.leg_id) { $f.leg_id } else { & $legOf $f.field })
      seq = $(if ($p -contains 'seq' -and $f.seq) { [int]$f.seq } else { 1 })
      value = & $str $f.value; unit = $(if ($p -contains 'unit') { & $str $f.unit } else { '' })
      value_status = $f.value_status
      source_page = $(if ($p -contains 'page') { & $str $f.page } else { '' })
      source_location = $(if ($p -contains 'location') { & $str $f.location } else { '' })
      quote = $(if ($p -contains 'quote') { & $str $f.quote } else { '' })
      copilot_value = $(if ($p -contains 'copilot_value') { & $str $f.copilot_value } else { '' })
      review_status = $f.review_status
      review_note = $(if ($p -contains 'review_note') { & $str $f.review_note } else { '' })
    } })
  $result = Join-Path $caseDir 'result'
  Write-C1Text (Join-Path $result 'conditions.csv') (($rows | ConvertTo-Csv -NoTypeInformation -UseQuotes Always) -join "`r`n") -Bom
  Write-C1Json (Join-Path $result 'conditions.json') ([ordered]@{ case_id = $CaseId; document = $document; reading = $manifest.reading; conditions = $rows })
  Copy-Item (Join-Path $caseDir 'agents\claude\fpml.xml') (Join-Path $result 'fpml.xml') -Force
  Copy-Item (Join-Path $caseDir 'agents\claude\review_items.md') (Join-Path $result 'review_items.md') -Force
  Write-C1Json (Join-Path $result 'evidence.json') ([ordered]@{ sharepoint = $version; copilot_attributions = @($cop.log['attributions'] | Where-Object { $_ }) })
  $manifest.stages.outputs = '保存済み（R の検証待ち）'
} catch [C1Failure] {
  $exit = $script:ExitCodes[$_.Exception.Kind]
  $manifest.error = $_.Exception.Message
  Write-C1Log '中止' $_.Exception.Message 'error'
} catch {
  $exit = $script:ExitCodes.error
  $manifest.error = $_.Exception.Message
  Write-C1Log '中止' $_.Exception.Message 'error'
} finally {
  $token = $null
}
$manifest.exit_code = $exit
$manifest.state = $script:StateOfExit[$exit]
$manifest.finished_at = (Get-C1Now)
$manifest.files = @(Get-ChildItem -Recurse -File $caseDir | ForEach-Object { $_.FullName.Substring($caseDir.Length + 1) -replace '\\', '/' })
Write-C1Json (Join-Path $caseDir 'manifest.json') $manifest
Write-C1Log '終了' ("状態：{0}（終了コード {1}）　案件フォルダ：{2}" -f $manifest.state, $exit, $caseDir)
# R が読む1行（案件フォルダ）
Write-Output "C1_CASE_DIR=$caseDir"
exit $exit
