param(
  [int]$Port = 3842,
  [string]$HostAddress = "127.0.0.1"
)

$ErrorActionPreference = "Stop"

$projectRoot = Split-Path -Parent (Split-Path -Parent $MyInvocation.MyCommand.Path)
Set-Location $projectRoot

$rscript = "C:\Program Files\R\R-4.6.0\bin\Rscript.exe"
if (-not (Test-Path $rscript)) {
  throw "Rscript was not found at $rscript"
}

& $rscript -e ".libPaths(c(normalizePath('r-lib'), .libPaths())); shiny::runApp('app', host='$HostAddress', port=$Port, launch.browser=FALSE)"
