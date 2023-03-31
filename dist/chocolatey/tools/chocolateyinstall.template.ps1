$ErrorActionPreference = 'Stop';
$toolsDir = "$(Split-Path -parent $MyInvocation.MyCommand.Definition)"

$packageArgs = @{
  packageName    = $env:ChocolateyPackageName
  destination    = $toolsDir
  fileFullPath   = Join-Path $toolsDir '{{ .X86_ZIPPath }}'
  fileFullPath64 = Join-Path $toolsDir '{{ .X64_ZIPPath }}'
}

Get-ChocolateyUnzip @packageArgs

Remove-Item -Force -Path $packageArgs.fileFullPath
Remove-Item -Force -Path $packageArgs.fileFullPath64
