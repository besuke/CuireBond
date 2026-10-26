<#
c1 URL 取得のテスト（HTTP 応答のモック）：ネットワークにつながず、Invoke-WebRequest を差し替えて確かめる。
  200（PDF・HTML）、403（User-Agent の案内）、404、追跡パラメータの除去、形式の判定、HTML の文字コード
CURIEプロジェクトの最上位フォルダで実行する：pwsh -File scripts/test_c1_http_mock.ps1
#>
. (Join-Path $PSScriptRoot '..\R\c1_trs_terms\PS\common.ps1')
$ErrorActionPreference = 'Stop'
$tmp = Join-Path ([IO.Path]::GetTempPath()) ('c1_http_mock_' + [guid]::NewGuid())
New-Item -ItemType Directory -Path $tmp | Out-Null

# モックの応答：URL の末尾で状態コード・種類・本文を決める
$pdfBytes = [IO.File]::ReadAllBytes((Resolve-Path (Join-Path $PSScriptRoot '..\data\demo_c1_trs\pdf\DEMO_TRS_confirmation.pdf')))
$cp1252 = [Text.Encoding]::GetEncoding(1252).GetBytes("<html><body><p>TOTAL RETURN SWAP CONFIRMATION</p><p>the 2000 ISDA Definitions (the `“Swap Definitions`”)</p><p>Common Stock (the &#147;Fund&#148;)</p></body></html>")
$responses = @{
  'ok.pdf'       = @{ status = 200; type = 'application/pdf'; body = $pdfBytes }
  'page.htm'     = @{ status = 200; type = 'text/html'; body = $cp1252 }
  'mislabel.bin' = @{ status = 200; type = 'application/octet-stream'; body = $pdfBytes }
  'denied.htm'   = @{ status = 403; type = 'text/html'; body = [Text.Encoding]::ASCII.GetBytes('Forbidden') }
  'missing.pdf'  = @{ status = 404; type = 'text/html'; body = [Text.Encoding]::ASCII.GetBytes('Not Found') }
}
$script:LastUserAgent = $null
function Invoke-WebRequest {
  param($Uri, $OutFile, [switch]$PassThru, $TimeoutSec, $Headers, [switch]$SkipHeaderValidation, [switch]$SkipHttpErrorCheck, $MaximumRedirection)
  $script:LastUserAgent = $Headers['User-Agent']
  $key = ([uri]$Uri).Segments[-1]
  $r = $responses[$key]
  [IO.File]::WriteAllBytes($OutFile, $r.body)
  [pscustomobject]@{ StatusCode = $r.status; Headers = @{ 'Content-Type' = @($r.type) }
                     BaseResponse = [pscustomobject]@{ RequestMessage = [pscustomobject]@{ RequestUri = [uri]$Uri } } }
}

$results = [System.Collections.Generic.List[object]]::new()
function Check([string]$Name, [scriptblock]$Test) {
  try { $ok = [bool](& $Test); $detail = '' } catch { $ok = $false; $detail = $_.Exception.Message }
  $results.Add([pscustomobject]@{ テスト = $Name; 判定 = $(if ($ok) { '合格' } else { '不合格' }); 詳細 = $detail })
}

$env:CURIE_HTTP_USER_AGENT = 'CURIE test (no contact)'
Check '200：PDF を取得し、形式は pdf' {
  $p = Join-Path $tmp 'a.bin'; $r = Save-C1PublicSource 'https://mock.example/ok.pdf' $p
  $r.http_status -eq 200 -and (Get-C1FormatRecord $p $r.content_type 'ok.pdf').kind -eq 'pdf'
}
Check 'User-Agent は環境変数の値を見出しで渡す' { $script:LastUserAgent -eq 'CURIE test (no contact)' }
Check '200：中身が PDF なら Content-Type が違っても pdf（判定根拠を記録）' {
  $p = Join-Path $tmp 'b.bin'; $r = Save-C1PublicSource 'https://mock.example/mislabel.bin' $p
  $f = Get-C1FormatRecord $p $r.content_type 'mislabel.bin'
  $f.kind -eq 'pdf' -and $f.by_content_type -eq $null -and $f.by_content -eq 'pdf'
}
Check '200：HTML（Windows-1252、文字参照 &#147; も）を文字化けせずに本文にする' {
  $p = Join-Path $tmp 'c.htm'; $r = Save-C1PublicSource 'https://mock.example/page.htm' $p
  $pages = @(Get-C1ReadingPages $p 'html' (Join-Path $tmp 'reading'))
  (Get-C1FormatRecord $p $r.content_type 'page.htm').kind -eq 'html' -and $pages.Count -eq 1 -and
    $pages[0].text -match '“Swap Definitions”' -and $pages[0].text -match '“Fund”' -and $pages[0].text -notmatch [char]0xFFFD
}
Remove-Item Env:CURIE_HTTP_USER_AGENT
Check '403：取得失敗。User-Agent の設定方法を案内し、ファイルを残さない' {
  $p = Join-Path $tmp 'd.bin'
  try { Save-C1PublicSource 'https://mock.example/denied.htm' $p; $false } catch {
    $_.Exception.Message -match 'HTTP 403' -and $_.Exception.Message -match 'CURIE_HTTP_USER_AGENT' -and -not (Test-Path $p) }
}
Check '404：取得失敗' {
  try { Save-C1PublicSource 'https://mock.example/missing.pdf' (Join-Path $tmp 'e.bin'); $false } catch { $_.Exception.Message -match 'HTTP 404' }
}
Check '追跡用パラメータ（utm_）だけを除く' {
  (Remove-C1TrackingParams 'https://mock.example/a.pdf?utm_source=x&id=3&utm_medium=y#p2') -eq 'https://mock.example/a.pdf?id=3#p2'
}

Remove-Item -Recurse -Force $tmp
$results | ForEach-Object { Write-Host ("{0}  {1}{2}" -f $_.判定, $_.テスト, $(if ($_.詳細) { "（" + $_.詳細 + "）" } else { "" })) }
$failed = @($results | Where-Object 判定 -ne '合格').Count
Write-Host ("{0}／{1} 件が期待どおり" -f ($results.Count - $failed), $results.Count)
exit $(if ($failed) { 1 } else { 0 })
