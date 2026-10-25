# c1 PowerShell 共通処理：ログ、終了コード、Microsoft への委任認証、Graph 呼び出し、共有URLからの資料取得。
#
# 認証：Microsoft ID プラットフォームのデバイスコード方式を Invoke-RestMethod で直接使う
#       （Microsoft.Graph.Authentication モジュールは、この PC では Smart App Control に DLL を止められたため使わない）。
#       アクセストークンは変数にだけ置き、ファイル・ログ・画面には出さない。クライアントシークレットは使わない。
# 接続情報：環境変数 CURIE_M365_TENANT_ID、CURIE_M365_CLIENT_ID（アプリ登録。公開クライアントフロー許可）。
#
# 参照した公式資料
#   デバイスコード方式 https://learn.microsoft.com/en-us/entra/identity-platform/v2-oauth2-device-code
#   サイト（パス指定） https://learn.microsoft.com/en-us/graph/api/site-getbypath
#   ドライブ一覧       https://learn.microsoft.com/en-us/graph/api/drive-list
#   driveItem の取得   https://learn.microsoft.com/en-us/graph/api/driveitem-get
#   共有リンク         https://learn.microsoft.com/en-us/graph/api/shares-get （最小権限は Files.ReadWrite）
#   内容のダウンロード https://learn.microsoft.com/en-us/graph/api/driveitem-get-content

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

# 終了コード（R はこの値で状態を判断する）
$script:ExitCodes = [ordered]@{
  ok                    = 0   # 成果物あり（R の検証待ち）
  error                 = 1   # 想定外のエラー
  not_connected         = 2   # 未接続（接続情報の不足・認証の失敗）
  document_access       = 3   # 資料にアクセスできない（取得・版の確認に失敗）
  copilot_access        = 4   # Copilot が資料を読めない・API の失敗
  copilot_format        = 5   # Copilot の回答形式が不正
  claude_access         = 6   # Claude が資料を読めない・実行の失敗
  claude_format         = 7   # Claude の回答形式が不正
}
$script:StateOfExit = @{ 0 = '要確認'; 1 = '失敗'; 2 = '未接続'; 3 = '読解失敗'; 4 = '読解失敗'; 5 = '回答形式不正'; 6 = '読解失敗'; 7 = '回答形式不正' }

class C1Failure : System.Exception {
  [string]$Kind
  C1Failure([string]$kind, [string]$message) : base($message) { $this.Kind = $kind }
}
function Stop-C1 ([string]$Kind, [string]$Message) { throw [C1Failure]::new($Kind, $Message) }

function Get-C1Now { (Get-Date).ToString('yyyy-MM-ddTHH:mm:sszzz') }

# UTF-8（BOMなし）で書く。CSV だけは Excel で開けるよう BOM 付き
function Write-C1Text([string]$Path, [string]$Text, [switch]$Bom) {
  $dir = Split-Path -Parent $Path
  if ($dir -and -not (Test-Path $dir)) { New-Item -ItemType Directory -Force -Path $dir | Out-Null }
  [System.IO.File]::WriteAllText($Path, $Text, [System.Text.UTF8Encoding]::new([bool]$Bom))
}
function Write-C1Json([string]$Path, $Object) { Write-C1Text $Path ($Object | ConvertTo-Json -Depth 30) }
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

# ---- 認証（デバイスコード方式） ----------------------------------------------------------
# Copilot Chat API に必要な委任権限（公式仕様。7つすべて必要）
$script:CopilotScopes = @('Sites.Read.All', 'Mail.Read', 'People.Read.All', 'OnlineMeetingTranscript.Read.All',
                          'Chat.Read', 'ChannelMessage.Read.All', 'ExternalItem.Read.All')

function Test-C1Connection {
  $missing = @('CURIE_M365_TENANT_ID', 'CURIE_M365_CLIENT_ID') | Where-Object { -not [Environment]::GetEnvironmentVariable($_) }
  return @($missing)
}

function Get-C1GraphToken([string[]]$Scopes) {
  $missing = Test-C1Connection
  if ($missing.Count) {
    Stop-C1 'not_connected' ("接続情報がありません: " + ($missing -join ', ') +
      "。テナントIDとアプリ登録のクライアントIDを環境変数に設定してください（demo には切り替えません）")
  }
  $tenant = $env:CURIE_M365_TENANT_ID; $client = $env:CURIE_M365_CLIENT_ID
  $base = "https://login.microsoftonline.com/$tenant/oauth2/v2.0"
  $scope = (($Scopes | ForEach-Object { "https://graph.microsoft.com/$_" }) -join ' ')
  try {
    $code = Invoke-RestMethod -Method Post -Uri "$base/devicecode" -Body @{ client_id = $client; scope = $scope }
  } catch { Stop-C1 'not_connected' ("サインインを開始できません: " + $_.Exception.Message) }
  # 利用者に表示する案内（URLと入力コード）。ログには書かない
  Write-Host $code.message -ForegroundColor Yellow
  $deadline = (Get-Date).AddSeconds([int]$code.expires_in); $interval = [int]$code.interval
  while ((Get-Date) -lt $deadline) {
    Start-Sleep -Seconds $interval
    try {
      $tok = Invoke-RestMethod -Method Post -Uri "$base/token" -Body @{
        grant_type = 'urn:ietf:params:oauth:grant-type:device_code'; client_id = $client; device_code = $code.device_code }
      return $tok.access_token
    } catch {
      $err = $null
      try { $err = ($_.ErrorDetails.Message | ConvertFrom-Json).error } catch { }
      if ($err -eq 'authorization_pending') { continue }
      if ($err -eq 'slow_down') { $interval += 5; continue }
      Stop-C1 'not_connected' ("サインインに失敗しました（" + ($err ?? $_.Exception.Message) + "）")
    }
  }
  Stop-C1 'not_connected' 'サインインの期限が切れました'
}

# Graph 呼び出し（トークンは見出しにだけ使い、例外文にも含めない）
function Invoke-C1Graph([string]$Method, [string]$Path, $Body = $null, [string]$Token, [int]$TimeoutSec = 120, [int[]]$Expect = @(200)) {
  $uri = if ($Path -match '^https://') { $Path } else { "https://graph.microsoft.com/$Path" }
  $params = @{ Method = $Method; Uri = $uri; Headers = @{ Authorization = "Bearer $Token" }; TimeoutSec = $TimeoutSec
               SkipHttpErrorCheck = $true; StatusCodeVariable = 'status' }
  if ($null -ne $Body) { $params.Body = [System.Text.Encoding]::UTF8.GetBytes(($Body | ConvertTo-Json -Depth 30 -Compress)); $params.ContentType = 'application/json; charset=utf-8' }
  $resp = Invoke-RestMethod @params
  if ($Expect -notcontains $status) {
    $detail = try { $resp.error.message } catch { '' }
    throw "Graph $Method $($uri -replace '\?.*$','') が HTTP $status（$detail）"
  }
  return $resp
}

# 共有URLの種類：共有リンク（/:b:/ など）か、ファイルへの直接のURLか
function Test-C1SharingLink([string]$Url) { return ([uri]$Url).AbsolutePath -match '^/:[a-z]:/' }
function Test-C1SharePointUrl([string]$Url) { return ([uri]$Url).Host -match '\.sharepoint\.com$' }

# 共有URL → driveItem（id・eTag・サイズ・更新日時・webUrl・ダウンロードURL）
function Get-C1DriveItem([string]$Url, [string]$Token) {
  $select = 'id,name,eTag,cTag,size,lastModifiedDateTime,webUrl,file,parentReference,@microsoft.graph.downloadUrl'
  if (Test-C1SharingLink $Url) {
    $b64 = [Convert]::ToBase64String([System.Text.Encoding]::UTF8.GetBytes($Url)).TrimEnd('=').Replace('/', '_').Replace('+', '-')
    return Invoke-C1Graph GET "v1.0/shares/u!$b64/driveItem?`$select=$select" -Token $Token
  }
  # 直接のURL：サイト（/sites/名前 または /teams/名前）→ ライブラリ（webUrl の前方一致）→ ライブラリ内のパス
  $u = [uri]$Url
  $segments = $u.AbsolutePath.Trim('/').Split('/')
  if ($segments.Count -lt 3 -or $segments[0] -notin @('sites', 'teams')) { throw "SharePoint のサイトを判定できないURLです: $Url" }
  $site = Invoke-C1Graph GET ("v1.0/sites/{0}:/{1}/{2}" -f $u.Host, $segments[0], $segments[1]) -Token $Token
  $drives = (Invoke-C1Graph GET "v1.0/sites/$($site.id)/drives?`$select=id,name,webUrl" -Token $Token).value
  $target = [uri]::UnescapeDataString($Url.Split('?')[0])
  $drive = $drives | Where-Object { $target.StartsWith([uri]::UnescapeDataString($_.webUrl) + '/') } | Select-Object -First 1
  if (-not $drive) { throw "URLに対応するドキュメントライブラリが見つかりません: $Url" }
  $rel = $target.Substring([uri]::UnescapeDataString($drive.webUrl).Length + 1)
  $encoded = ($rel.Split('/') | ForEach-Object { [uri]::EscapeDataString($_) }) -join '/'
  return Invoke-C1Graph GET "v1.0/drives/$($drive.id)/root:/$encoded`?`$select=$select" -Token $Token
}

# 事前認証済みのダウンロードURLから取得（トークンは付けない。URLは短時間で失効するため記録しない）
function Save-C1DriveItemContent($Item, [string]$Path) {
  $url = $Item.'@microsoft.graph.downloadUrl'
  if (-not $url) { throw 'ダウンロードURLを取得できませんでした' }
  Invoke-WebRequest -Uri $url -OutFile $Path -TimeoutSec 300 | Out-Null
}

# 版の記録（R の照合に使う）
function Get-C1VersionRecord($Item, [string]$ContentPath) {
  [ordered]@{ item_id = $Item.id; name = $Item.name; web_url = $Item.webUrl; e_tag = $Item.eTag; c_tag = $Item.cTag
              size = $Item.size; last_modified = $Item.lastModifiedDateTime
              sha256 = if ($ContentPath) { Get-C1Sha256 $ContentPath } else { $null }; checked_at = (Get-C1Now) }
}

# 文書の種類（中身で判定）。HTML は Chrome／Edge のヘッドレス印刷で PDF にする
function Get-C1DocumentKind([string]$Path) {
  $bytes = [System.IO.File]::ReadAllBytes($Path) | Select-Object -First 2048
  if ($bytes.Count -ge 5 -and [System.Text.Encoding]::ASCII.GetString($bytes[0..4]) -eq '%PDF-') { return 'pdf' }
  $text = [System.Text.Encoding]::UTF8.GetString($bytes).ToLower()
  if ($text -match '<html|<!doctype html|<body|<document>') { return 'html' }
  return $null
}
function Convert-C1HtmlToPdf([string]$HtmlPath, [string]$PdfPath) {
  $browser = @($env:CURIE_CHROME, "$env:ProgramFiles\Google\Chrome\Application\chrome.exe",
               "${env:ProgramFiles(x86)}\Microsoft\Edge\Application\msedge.exe") | Where-Object { $_ -and (Test-Path $_) } | Select-Object -First 1
  if (-not $browser) { throw 'HTML を PDF にするブラウザ（Chrome／Edge）が見つかりません' }
  $profileDir = Join-Path ([System.IO.Path]::GetTempPath()) ("curie_browser_" + [guid]::NewGuid())
  try {
    $fileUrl = 'file:///' + ((Resolve-Path $HtmlPath).Path -replace '\\', '/')
    Start-Process -FilePath $browser -Wait -WindowStyle Hidden -ArgumentList @('--headless=new', '--disable-gpu', '--no-pdf-header-footer',
      "--user-data-dir=`"$profileDir`"", "--print-to-pdf=`"$PdfPath`"", "`"$fileUrl`"")
  } finally { Remove-Item -Recurse -Force $profileDir -ErrorAction SilentlyContinue }
  if (-not (Test-Path $PdfPath)) { throw "HTML を PDF に変換できませんでした: $HtmlPath" }
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
