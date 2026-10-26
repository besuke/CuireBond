<#
c1：資料1件の読解（PowerShell の入口。単独でも、R から起動しても動く）。

  入力：公開 PDF・HTML の URL、ローカルの PDF・HTML（Box Drive の同期フォルダのファイルも可）
        形式は中身・Content-Type・拡張子で判定する
  流れ：原資料の取得と記録 → 読解用データ（本文。必要なら OCR）→ Claude の読解・FpML 作成 → JSON・FpML の保存
  成果物：output/c1_trs/cases/<案件ID>/result/ の conditions.json と fpml.xml（このスクリプトのフォルダには保存しない）
  終了コード：0 成果物あり（R の検証待ち）、3 取得失敗、6 Claude 読解失敗、7 Claude 回答形式不正、
              8 読解用データ作成失敗、1 想定外のエラー

例：
  pwsh -File R/c1_trs_terms/PS/run_document.ps1 -Source "https://www.sec.gov/Archives/edgar/data/1141719/000095013003002179/dex1019.htm"
  pwsh -File R/c1_trs_terms/PS/run_document.ps1 -Source data/demo_c1_trs/pdf/DEMO_TRS_confirmation.pdf -ClaudeMode demo -ClaudeDemoDir <フォルダ>
#>
param(
  [Parameter(Mandatory)][string]$Source,
  [ValidateSet('api', 'demo')][string]$ClaudeMode = 'api',
  [string]$CaseId,
  [string]$OutputRoot,
  [string]$ClaudeDemoDir,
  [string]$InstructionPath
)

. (Join-Path $PSScriptRoot 'common.ps1')
. (Join-Path $PSScriptRoot 'invoke_claude.ps1')

$repo = (Resolve-Path (Join-Path $PSScriptRoot '..\..\..')).Path
if (-not $OutputRoot) { $OutputRoot = Join-Path $repo 'output\c1_trs\cases' }
if (-not $InstructionPath) { $InstructionPath = Join-Path $repo 'docs\c1_trs_extraction_instructions.md' }
$pasted = $Source.Trim().Trim('"')
$source = Remove-C1TrackingParams $pasted   # utm_ で始まる追跡用パラメータを除く（元の入力も記録する）
$isUrl = $source -match '^https?://'
$name = if ($isUrl) { [uri]::UnescapeDataString(([uri]$source).Segments[-1]) } else { Split-Path -Leaf $source }
$stem = ([IO.Path]::GetFileNameWithoutExtension($name) -replace '[^A-Za-z0-9_-]', '_')
if (-not $stem) { $stem = 'document' }
if (-not $CaseId) { $CaseId = $stem + '_' + (Get-Date -Format 'yyyyMMdd_HHmmss') }
$caseDir = Join-Path $OutputRoot $CaseId
foreach ($d in 'source', 'agents', 'result', 'logs') { New-Item -ItemType Directory -Force -Path (Join-Path $caseDir $d) | Out-Null }
$script:C1LogPath = Join-Path $caseDir 'logs\run_log.jsonl'

$sourceType = if ($isUrl) { 'public_url' } else { 'local_file' }
$manifest = [ordered]@{
  case_id = $CaseId; created_at = (Get-C1Now); source = $source; pasted_input = $pasted; source_type = $sourceType
  claude_mode = $ClaudeMode
  stages = [ordered]@{ document = '未実行'; reading_data = '未実行'; claude = '未実行'; outputs = '未実行' }
}
$exit = $script:ExitCodes.ok
try {
  # ---- 1. 原資料の取得と記録（取得元・取得日時・ハッシュ・形式の判定根拠）-------------------
  Write-C1Log '資料の取得' ("{0}（{1}）" -f $source, @{ public_url = '公開URL'; local_file = 'ローカルファイル' }[$sourceType])
  $raw = Join-Path $caseDir 'source\original.bin'
  $retrievedAt = Get-C1Now
  try { $http = Save-C1PublicSource $source $raw }
  catch { Stop-C1 'document_access' ("資料を取得できません（取得失敗）: " + $_.Exception.Message) }
  $format = Get-C1FormatRecord $raw $http.content_type $name
  if (-not $format.kind) { Stop-C1 'document_access' ("PDF でも HTML でもありません（Content-Type：{0}、拡張子：{1}）" -f $http.content_type, $format.extension) }
  $ext = if ($format.kind -eq 'pdf') { '.pdf' } else { '.htm' }
  $original = Join-Path $caseDir ("source\original" + $ext)
  Move-Item -LiteralPath $raw -Destination $original -Force
  $sha = Get-C1Sha256 $original
  $acquisition = [ordered]@{ source = $source; pasted_input = $pasted; source_type = $sourceType; retrieved_at = $retrievedAt
                             http_status = $http.http_status; content_type = $http.content_type; final_url = $http.final_url
                             format = $format; stored_as = ("source/original" + $ext); bytes = (Get-Item $original).Length; sha256 = $sha }
  Write-C1Json (Join-Path $caseDir 'source\acquisition.json') $acquisition
  $manifest.acquisition = $acquisition
  $manifest.stages.document = '取得済み'

  # ---- 2. 読解用データ（本文。ページごと。PDF で文字のないページは OCR）--------------------------
  #      R の引用照合とページ数の記録に使う。Claude には原資料そのものを渡す
  try { $pagesData = @(Get-C1ReadingPages $original $format.kind (Join-Path $caseDir 'source\reading')) }
  catch { Stop-C1 'reading_data' ("読解用データを作れません: " + $_.Exception.Message) }
  $textChars = ($pagesData | Where-Object { $_.method -ne 'ocr_failed' } | ForEach-Object { ($_.text -replace '\s', '').Length } | Measure-Object -Sum).Sum
  if (-not $textChars) { Stop-C1 'reading_data' '本文を抜き出せず、OCR でも文字を読めませんでした' }
  Write-C1Json (Join-Path $caseDir 'source\reading_pages.json') @($pagesData)
  Write-C1Text (Join-Path $caseDir 'source\reading_text.txt') ((@($pagesData) | ForEach-Object { "===== p.$($_.page)（$($_.method)）=====`n$($_.text)" }) -join "`n`n")
  $methods = (@($pagesData) | Group-Object { $_.method } | ForEach-Object { "$($_.Name) $($_.Count) ページ" }) -join '、'
  $manifest.reading_data = [ordered]@{ pages = @($pagesData).Count; methods = $methods; text_chars = $textChars
                                       sha256 = (Get-C1Sha256 (Join-Path $caseDir 'source\reading_text.txt')) }
  $manifest.stages.reading_data = "作成済み（$methods）"
  $document = [ordered]@{ document_id = $stem; file_name = $name; pages = @($pagesData).Count; sha256 = $sha
                          kind = $format.kind; local_name = ('document' + $ext); source_type = $sourceType }
  $manifest.document = $document
  Write-C1Log '資料の取得' ("{0}（{1}、{2} ページ、SHA-256 {3}…、本文 {4}）" -f $name, $format.kind.ToUpper(), $document.pages, $sha.Substring(0, 12), $methods)

  # ---- 3. Claude の読解・FpML 作成（取得した原資料を渡す）----------------------------------------
  $cla = Invoke-C1Claude -Mode $ClaudeMode -Document $document -SourcePath $original `
    -AgentDir (Join-Path $caseDir 'agents\claude') -InstructionPath $InstructionPath -DemoDir $ClaudeDemoDir
  $manifest.claude = $cla.log
  $manifest.stages.claude = @{ ok = '取得済み'; access_failed = '読解失敗'; invalid = '回答形式不正' }[$cla.state]
  if ($ClaudeMode -eq 'demo') { $manifest.stages.claude += '（デモ：Claude は呼んでいない）' }
  if ($cla.state -eq 'access_failed') { Stop-C1 'claude_access' $cla.log.error }
  if ($cla.state -eq 'invalid') { Stop-C1 'claude_format' $cla.log.error }

  # ---- 4. 成果物：conditions.json（1項目1件）と fpml.xml ----------------------------------------
  Write-C1Log '成果物の保存' (Join-Path $caseDir 'result')
  $legOf = { param($f) switch -Regex ($f) { '^return\.' { 'return_leg' } '^funding\.' { 'interest_leg' } '^schedule\.' { 'schedule' } default { 'trade' } } }
  $str = { param($v) if ($null -eq $v) { '' } elseif ($v -is [array]) { ($v | ForEach-Object { "$_" }) -join ' / ' } else { "$v" } }
  $rows = @(@($cla.reviewed.fields) | ForEach-Object {
    $f = $_; $p = $f.PSObject.Properties.Name
    [ordered]@{
      case_id = $CaseId; item_id = $f.field
      leg_id = $(if ($p -contains 'leg_id' -and $f.leg_id) { $f.leg_id } else { & $legOf $f.field })
      seq = $(if ($p -contains 'seq' -and $f.seq) { [int]$f.seq } else { 1 })
      value = & $str $f.value; unit = $(if ($p -contains 'unit') { & $str $f.unit } else { '' })
      value_status = $f.value_status
      source_page = $(if ($p -contains 'page') { & $str $f.page } else { '' })
      source_location = $(if ($p -contains 'location') { & $str $f.location } else { '' })
      quote = $(if ($p -contains 'quote') { & $str $f.quote } else { '' })
      review_status = $f.review_status
      review_note = $(if ($p -contains 'review_note') { & $str $f.review_note } else { '' })
    } })
  $result = Join-Path $caseDir 'result'
  $reviewText = Get-Content -LiteralPath (Join-Path $caseDir 'agents\claude\review_items.md') -Raw -Encoding utf8 -ErrorAction SilentlyContinue
  Write-C1Json (Join-Path $result 'conditions.json') ([ordered]@{
    case_id = $CaseId; document = $document; acquisition = $acquisition; reading_data = $manifest.reading_data
    review_items = ($reviewText ?? ''); conditions = $rows })
  Copy-Item (Join-Path $caseDir 'agents\claude\fpml.xml') (Join-Path $result 'fpml.xml') -Force
  $manifest.stages.outputs = '保存済み（R の検証待ち）'
} catch [C1Failure] {
  $exit = $script:ExitCodes[$_.Exception.Kind]
  $manifest.error = $_.Exception.Message
  switch ($_.Exception.Kind) {
    'document_access' { $manifest.stages.document = '取得失敗' }
    'reading_data'    { $manifest.stages.reading_data = '作成失敗' }
  }
  Write-C1Log '中止' $_.Exception.Message 'error'
} catch {
  $exit = $script:ExitCodes.error
  $manifest.error = $_.Exception.Message
  Write-C1Log '中止' $_.Exception.Message 'error'
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
