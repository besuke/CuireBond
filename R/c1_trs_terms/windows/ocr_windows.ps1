<#
c1：Windows 標準の OCR（Windows.Media.Ocr）で画像の文字を読む。追加のソフトは不要。
WinRT を使うため Windows PowerShell 5.1（powershell.exe）で実行する。c1 の R（reading_data.R）が、文字のない PDF ページについてだけ呼ぶ。
言語は利用者のプロフィールの言語（日本語の Windows なら日本語と英語など）。結果は UTF-8 のテキストファイルに書く。

  powershell.exe -NoProfile -ExecutionPolicy Bypass -File ocr_windows.ps1 -ImagePath p1.png -OutPath p1.txt
#>
param([Parameter(Mandatory)][string]$ImagePath, [Parameter(Mandatory)][string]$OutPath)
$ErrorActionPreference = 'Stop'
Add-Type -AssemblyName System.Runtime.WindowsRuntime
$null = [Windows.Storage.StorageFile, Windows.Storage, ContentType = WindowsRuntime]
$null = [Windows.Media.Ocr.OcrEngine, Windows.Foundation, ContentType = WindowsRuntime]
$null = [Windows.Graphics.Imaging.BitmapDecoder, Windows.Graphics, ContentType = WindowsRuntime]

# WinRT の非同期処理を待つ
$asTask = ([System.WindowsRuntimeSystemExtensions].GetMethods() | Where-Object {
  $_.Name -eq 'AsTask' -and $_.GetParameters().Count -eq 1 -and $_.GetParameters()[0].ParameterType.Name -eq 'IAsyncOperation`1' })[0]
function Await($op, [type]$type) {
  $task = $asTask.MakeGenericMethod($type).Invoke($null, @($op))
  $task.Wait() | Out-Null
  $task.Result
}

$engine = [Windows.Media.Ocr.OcrEngine]::TryCreateFromUserProfileLanguages()
if (-not $engine) { throw 'Windows の OCR エンジンを作れません（OCR の言語が入っていない可能性）' }
$file = Await ([Windows.Storage.StorageFile]::GetFileFromPathAsync((Resolve-Path $ImagePath).Path)) ([Windows.Storage.StorageFile])
$stream = Await ($file.OpenAsync([Windows.Storage.FileAccessMode]::Read)) ([Windows.Storage.Streams.IRandomAccessStream])
$decoder = Await ([Windows.Graphics.Imaging.BitmapDecoder]::CreateAsync($stream)) ([Windows.Graphics.Imaging.BitmapDecoder])
$bitmap = Await ($decoder.GetSoftwareBitmapAsync()) ([Windows.Graphics.Imaging.SoftwareBitmap])
$result = Await ($engine.RecognizeAsync($bitmap)) ([Windows.Media.Ocr.OcrResult])
$lines = @($result.Lines | ForEach-Object { $_.Text })
[System.IO.File]::WriteAllText($OutPath, ($lines -join "`n"), [System.Text.UTF8Encoding]::new($false))
Write-Output ("OCR_LANG=" + $engine.RecognizerLanguage.LanguageTag)
