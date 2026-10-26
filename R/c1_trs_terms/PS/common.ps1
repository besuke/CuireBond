# c1 PowerShell 共通処理：ログ、終了コード、資料の取得（公開URL・ローカルファイル）、読解用データ、回答の形式チェック。

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
# 画面出力は UTF-8 に固定する（RStudio から起動するとコンソールがなく、既定が CP932 になって文字化けするため）
[Console]::OutputEncoding = [System.Text.UTF8Encoding]::new($false)
$OutputEncoding = [System.Text.UTF8Encoding]::new($false)

# 終了コード（R はこの値で状態を判断する。番号は以前の版と同じ）
$script:ExitCodes = [ordered]@{
  ok              = 0   # 成果物あり（R の検証待ち）
  error           = 1   # 想定外のエラー
  document_access = 3   # 取得失敗（資料を取得できない）
  claude_access   = 6   # Claude 読解失敗（資料を読めない・実行の失敗）
  claude_format   = 7   # Claude の回答形式が不正
  reading_data    = 8   # 読解用データを作れない（本文抽出・OCR の失敗）
}
$script:StateOfExit = @{ 0 = '要確認'; 1 = '失敗'; 3 = '取得失敗'; 6 = '読解失敗'; 7 = '回答形式不正'; 8 = '読解用データ作成失敗' }

class C1Failure : System.Exception {
  [string]$Kind
  C1Failure([string]$kind, [string]$message) : base($message) { $this.Kind = $kind }
}
function Stop-C1 ([string]$Kind, [string]$Message) { throw [C1Failure]::new($Kind, $Message) }

function Get-C1Now { (Get-Date).ToString('yyyy-MM-ddTHH:mm:sszzz') }

# UTF-8（BOMなし）で書く
function Write-C1Text([string]$Path, [string]$Text, [switch]$Bom) {
  $dir = Split-Path -Parent $Path
  if ($dir -and -not (Test-Path $dir)) { New-Item -ItemType Directory -Force -Path $dir | Out-Null }
  [System.IO.File]::WriteAllText($Path, $Text, [System.Text.UTF8Encoding]::new([bool]$Bom))
}
function Write-C1Json([string]$Path, $Object) { Write-C1Text $Path (ConvertTo-Json -InputObject $Object -Depth 30) }   # 要素1つの配列も配列のまま書く
function Read-C1Json([string]$Path) { Get-Content -LiteralPath $Path -Raw -Encoding utf8 | ConvertFrom-Json -Depth 30 }
function Get-C1Sha256([string]$Path) { (Get-FileHash -LiteralPath $Path -Algorithm SHA256).Hash.ToLower() }

# 段階ごとのログ（画面と logs/run_log.jsonl）。秘密情報は渡さないこと
function Write-C1Log([string]$Stage, [string]$Message, [string]$Level = 'info') {
  $line = '{0} [c1-ps] {1}：{2}' -f (Get-Date -Format 'HH:mm:ss'), $Stage, $Message
  if ($Level -eq 'error') { Write-Host $line -ForegroundColor Red } else { Write-Host $line }
  if ($script:C1LogPath) {
    $rec = [ordered]@{ time = (Get-C1Now); stage = $Stage; level = $Level; message = $Message } | ConvertTo-Json -Compress
    [System.IO.File]::AppendAllText($script:C1LogPath, $rec + "`n", [System.Text.UTF8Encoding]::new($false))
  }
}

# 文書の種類（中身で判定）。PDF・HTML 以外は扱わない
function Get-C1DocumentKind([string]$Path) {
  $bytes = [System.IO.File]::ReadAllBytes($Path) | Select-Object -First 2048
  if ($bytes.Count -ge 5 -and [System.Text.Encoding]::ASCII.GetString($bytes[0..4]) -eq '%PDF-') { return 'pdf' }
  $text = [System.Text.Encoding]::UTF8.GetString($bytes).ToLower()
  if ($text -match '<html|<!doctype html|<body|<document>') { return 'html' }
  return $null
}

# 回答の文字から JSON を取り出す（```json ブロックを優先。なければ最初の { から最後の } まで）
function Get-C1JsonFromText([string]$Text) {
  if (-not $Text) { return $null }
  $blocks = [regex]::Matches($Text, '```(?:json)?\s*\n(.*?)\n\s*```', 'Singleline')
  if ($blocks.Count) { return $blocks[$blocks.Count - 1].Groups[1].Value }
  $s = $Text.IndexOf('{'); $e = $Text.LastIndexOf('}')
  if ($s -ge 0 -and $e -gt $s) { return $Text.Substring($s, $e - $s + 1) }
  return $null
}

# 抽出JSONの形式チェック（PowerShell 側は形だけ。条件の中身は R が検証する）
function Test-C1ExtractionShape($Obj, [string[]]$ExtraRequired = @()) {
  $issues = [System.Collections.Generic.List[string]]::new()
  if (-not $Obj) { $issues.Add('JSONではない'); return $issues }
  if (-not ($Obj.PSObject.Properties.Name -contains 'fields') -or -not $Obj.fields -or @($Obj.fields).Count -eq 0) { $issues.Add('fields がない'); return $issues }
  $i = 0
  foreach ($f in @($Obj.fields)) {
    $i++
    $names = $f.PSObject.Properties.Name
    foreach ($k in @('field', 'value_status') + $ExtraRequired) { if ($names -notcontains $k) { $issues.Add("fields[$i] に $k がない") } }
    if ($names -contains 'value_status' -and $f.value_status -notin @('stated', 'blank', 'bracketed', 'not_found')) { $issues.Add("fields[$i] の value_status が不正: $($f.value_status)") }
  }
  return $issues
}

# poppler（pdftoppm・pdfinfo）を PATH に加える。R や RStudio が poppler の導入前に起動していても見つかるように、
# 環境変数 CURIE_POPPLER_BIN → 利用者の PATH（レジストリ）→ winget の導入先 の順に探す
function Add-C1PopplerPath {
  if (Get-Command pdftoppm -ErrorAction SilentlyContinue) { return $true }
  $candidates = @($env:CURIE_POPPLER_BIN) + @(([Environment]::GetEnvironmentVariable('Path', 'User') ?? '').Split(';')) +
    @(Get-ChildItem -Directory -ErrorAction SilentlyContinue (Join-Path $env:LOCALAPPDATA 'Microsoft\WinGet\Packages') -Filter 'oschwartz10612.Poppler*' |
      ForEach-Object { Get-ChildItem -Directory $_.FullName -Filter 'poppler-*' } | ForEach-Object { Join-Path $_.FullName 'Library\bin' })
  $bin = $candidates | Where-Object { $_ -and (Test-Path (Join-Path $_ 'pdftoppm.exe')) } | Select-Object -First 1
  if ($bin) { $env:Path = "$bin;$env:Path"; return $true }
  return $false
}

# ---- 入力資料の取得（公開URL・ローカルファイル）-------------------------------------------
# URLから追跡用のパラメータ（utm_ で始まるもの）を除く。ほかのパラメータは残す
function Remove-C1TrackingParams([string]$Url) {
  if ($Url -notmatch '^https?://' -or $Url -notmatch '\?') { return $Url }
  $fragment = if ($Url -match '#') { $Url.Substring($Url.IndexOf('#')) } else { '' }
  $base = $Url.Split('?')[0]
  $query = $Url.Substring($Url.IndexOf('?') + 1).Split('#')[0]
  $kept = @($query.Split('&') | Where-Object { $_ -and $_ -notmatch '^utm_' })
  return $base + $(if ($kept.Count) { '?' + ($kept -join '&') } else { '' }) + $fragment
}

# 形式の判定：中身（先頭のバイト）を優先し、Content-Type と拡張子も記録する
function Get-C1FormatRecord([string]$Path, [string]$ContentType, [string]$NameOrUrl) {
  $magic = Get-C1DocumentKind $Path
  $ext = [IO.Path]::GetExtension(($NameOrUrl -split '[?#]')[0]).ToLower()
  $byExt = switch -Regex ($ext) { '^\.pdf$' { 'pdf' } '^\.html?$' { 'html' } default { $null } }
  $byType = switch -Regex ($ContentType ?? '') { 'application/pdf' { 'pdf' } 'text/html|application/xhtml' { 'html' } default { $null } }
  $kind = $magic ?? $byType ?? $byExt
  [ordered]@{ kind = $kind; by_content = $magic; by_content_type = $byType; by_extension = $byExt
              content_type = $ContentType; extension = $ext
              note = if ($magic -and (($byType -and $byType -ne $magic) -or ($byExt -and $byExt -ne $magic))) { '中身と Content-Type／拡張子が異なる（中身で判定）' } else { '' } }
}

# 公開URL・ローカルファイルの取得（アクセス制限は回避しない。取得できなければ理由を返す）
function Save-C1PublicSource([string]$Source, [string]$Path) {
  $rec = [ordered]@{ http_status = $null; content_type = $null; final_url = $null }
  if ($Source -match '^https?://') {
    $ua = $env:CURIE_HTTP_USER_AGENT ?? 'CURIE-c1 (TRS terms reader)'
    # User-Agent は見出しとして渡す（-UserAgent は値の形式を厳しく確かめ、メールアドレスを含む値を受け付けないため）
    $resp = Invoke-WebRequest -Uri $Source -OutFile $Path -PassThru -TimeoutSec 300 -Headers @{ "User-Agent" = $ua } -SkipHeaderValidation -SkipHttpErrorCheck -MaximumRedirection 5
    $rec.http_status = [int]$resp.StatusCode
    $rec.content_type = ($resp.Headers['Content-Type'] | Select-Object -First 1)
    $rec.final_url = $resp.BaseResponse.RequestMessage.RequestUri.AbsoluteUri
    if ($rec.http_status -ne 200) {
      $hint = if ($rec.http_status -eq 403 -and -not $env:CURIE_HTTP_USER_AGENT) {
        '（アクセスを拒否されました）。公開元によっては、名前と連絡先メールを含む User-Agent を求めます（SEC など）。その場合は環境変数 CURIE_HTTP_USER_AGENT="氏名 メールアドレス" を設定してください。アクセス制限の回避はしません'
      } elseif ($rec.http_status -in 401, 403) { '。アクセスが許可されていません（アクセス制限の回避はしません）' } else { '' }
      Remove-Item -LiteralPath $Path -ErrorAction SilentlyContinue
      throw ("HTTP {0}{1}" -f $rec.http_status, $hint)
    }
  } else {
    if (-not (Test-Path -LiteralPath $Source)) { throw "ファイルがありません: $Source" }
    Copy-Item -LiteralPath $Source -Destination $Path -Force
  }
  return $rec
}

# ---- 読解用データ（本文。ページごと）------------------------------------------------------
# PDF：poppler の pdftotext でページごとに抜き出す。文字がほとんどないページは Windows 標準の OCR で読む
# HTML：script・style を除き、段落・行・セルの境目を改行にして本文にする（全体を1ページとする）
function Get-C1ReadingPages([string]$Path, [string]$Kind, [string]$WorkDir, [int]$MinChars = 20) {
  New-Item -ItemType Directory -Force -Path $WorkDir | Out-Null
  if ($Kind -eq 'html') {
    $html = Read-C1HtmlText $Path
    $html = [regex]::Replace($html, '(?is)<(script|style)[^>]*>.*?</\1>', ' ')
    $html = [regex]::Replace($html, '(?i)<\s*(br|/p|/div|/tr|/li|/h[1-6]|/table)\b[^>]*>', "`n")
    $html = [regex]::Replace($html, '(?i)</t[dh]\s*>', "`t")
    $text = [System.Net.WebUtility]::HtmlDecode([regex]::Replace($html, '<[^>]+>', ' '))
    # 古い HTML の &#128;〜&#159;（例：&#147; &#148;）は、ブラウザと同じく Windows-1252 の文字（“ ” など）として読む
    $cp1252 = [System.Text.Encoding]::GetEncoding(1252)
    $text = [regex]::Replace($text, '[\u0080-\u009F]', { param($m) $cp1252.GetString([byte[]]@([int][char]$m.Value)) })
    $text = (($text -split "`n" | ForEach-Object { ($_ -replace '[ \t ]+', ' ').Trim() } | Where-Object { $_ }) -join "`n")
    return @([ordered]@{ page = 1; method = 'html_text'; chars = $text.Length; text = $text })
  }
  if (-not (Add-C1PopplerPath)) { throw 'poppler（pdftotext・pdftoppm）がないため、PDF の本文を抜き出せません' }
  $txt = Join-Path $WorkDir 'pdftotext.txt'
  & pdftotext -layout -enc UTF-8 $Path $txt 2>$null
  if ($LASTEXITCODE -ne 0 -or -not (Test-Path $txt)) { throw "pdftotext が失敗しました（終了コード $LASTEXITCODE）" }
  $pages = (Get-Content -LiteralPath $txt -Raw -Encoding utf8) -split "`f"
  if ($pages.Count -gt 1 -and -not $pages[-1].Trim()) { $pages = $pages[0..($pages.Count - 2)] }
  $ocr = Join-Path $PSScriptRoot 'ocr_windows.ps1'
  $i = 0
  return @($pages | ForEach-Object {
    $i++
    $text = $_.TrimEnd()
    $method = 'pdf_text'
    if (($text -replace '\s', '').Length -lt $MinChars) {
      # 文字のないページ（スキャン画像など）は OCR。結果には誤読がありうるため、method に記録する
      $png = Join-Path $WorkDir ("ocr_p{0}" -f $i)
      & pdftoppm -png -r 300 -f $i -l $i -singlefile $Path $png 2>$null
      $out = "$png.txt"
      $null = & powershell.exe -NoProfile -ExecutionPolicy Bypass -File $ocr -ImagePath "$png.png" -OutPath $out 2>&1
      if ($LASTEXITCODE -eq 0 -and (Test-Path $out)) { $text = Get-Content -LiteralPath $out -Raw -Encoding utf8; $method = 'ocr_windows' }
      else { $method = 'ocr_failed' }
    }
    [ordered]@{ page = $i; method = $method; chars = ($text ?? '').Length; text = ($text ?? '') }
  })
}

# HTML の文字コード：HTML の宣言（meta charset）→ UTF-8 として正しいか → Windows-1252（古い英文 HTML に多い）の順に決める
function Read-C1HtmlText([string]$Path) {
  $bytes = [System.IO.File]::ReadAllBytes($Path)
  $head = [System.Text.Encoding]::ASCII.GetString($bytes, 0, [Math]::Min(4096, $bytes.Length))
  $m = [regex]::Match($head, '(?i)charset\s*=\s*["'']?([A-Za-z0-9_-]+)')
  if ($m.Success) {
    try { return [System.Text.Encoding]::GetEncoding($m.Groups[1].Value).GetString($bytes) } catch { }
  }
  try { return [System.Text.UTF8Encoding]::new($false, $true).GetString($bytes) }   # 不正なバイトがあれば例外
  catch { return [System.Text.Encoding]::GetEncoding(1252).GetString($bytes) }
}
