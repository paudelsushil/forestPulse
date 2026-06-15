#requires -Version 5.1
<#
.SYNOPSIS
  Developer / release task runner for the forestPulse R package.

.DESCRIPTION
  Wraps the common devtools workflow (document, test, check, build, install)
  behind one script so the test -> production cycle is reproducible and the
  same on every machine. Locates Rscript automatically.

.EXAMPLE
  .\dev.ps1            # document + test + check  (the everyday loop)

.EXAMPLE
  .\dev.ps1 check-cran # strict CRAN check before a release

.EXAMPLE
  .\dev.ps1 build      # build the source tarball

.NOTES
  If PowerShell blocks the script, run it as:
    powershell -ExecutionPolicy Bypass -File .\dev.ps1 <task>
#>
[CmdletBinding()]
param(
  [Parameter(Position = 0)]
  [ValidateSet('setup', 'document', 'test', 'check', 'check-cran',
               'build', 'install', 'spell', 'all', 'help')]
  [string]$Task = 'all'
)

$ErrorActionPreference = 'Stop'
Set-Location -LiteralPath $PSScriptRoot

# --- locate Rscript --------------------------------------------------------
function Get-Rscript {
  $cmd = Get-Command Rscript -ErrorAction SilentlyContinue
  if ($cmd) { return $cmd.Source }
  $found = Get-ChildItem 'C:\Program Files\R\R-*\bin\Rscript.exe' -ErrorAction SilentlyContinue |
           Sort-Object FullName -Descending
  if ($found) { return $found[0].FullName }
  throw "Rscript not found. Install R, or add Rscript to PATH."
}
$Rscript = Get-Rscript

# --- run a snippet of R, fail the script on a non-zero exit ----------------
function Invoke-R([string]$Code, [string]$Label) {
  Write-Host ""
  Write-Host "=== $Label ===" -ForegroundColor Cyan
  & $Rscript -e $Code
  if ($LASTEXITCODE -ne 0) {
    Write-Host "FAILED: $Label (exit $LASTEXITCODE)" -ForegroundColor Red
    exit $LASTEXITCODE
  }
}

switch ($Task) {
  'setup' {
    Invoke-R "install.packages(c('devtools','roxygen2','testthat','spelling'), repos='https://cloud.r-project.org'); devtools::install_deps(dependencies = TRUE)" `
             'Install tooling + package dependencies'
  }
  'document' {
    Invoke-R "devtools::document()" 'Document (roxygen2 -> NAMESPACE, man/)'
  }
  'test' {
    Invoke-R "devtools::test()" 'Run testthat tests'
  }
  'check' {
    Invoke-R "devtools::check(error_on = 'warning')" 'R CMD check'
  }
  'check-cran' {
    Invoke-R "devtools::check(args = '--as-cran', error_on = 'warning')" 'R CMD check (--as-cran)'
  }
  'build' {
    Invoke-R "cat('Built:', devtools::build(), '\n')" 'Build source tarball'
  }
  'install' {
    Invoke-R "devtools::install(upgrade = FALSE)" 'Install package into library'
  }
  'spell' {
    Invoke-R "print(spelling::spell_check_package())" 'Spell-check'
  }
  'all' {
    Invoke-R "devtools::document()" 'Document'
    Invoke-R "devtools::test()" 'Test'
    Invoke-R "devtools::check(error_on = 'warning')" 'Check'
    Write-Host ""
    Write-Host "All tasks passed." -ForegroundColor Green
  }
  'help' {
    Write-Host @"
forestPulse task runner

  .\dev.ps1 <task>

Tasks:
  setup        Install devtools/roxygen2/testthat/spelling + package deps
  document     Regenerate NAMESPACE and man/*.Rd from roxygen comments
  test         Run the testthat test suite
  check        R CMD check (fails on warning or error)
  check-cran   R CMD check --as-cran (stricter; run before a release)
  build        Build the source tarball (../forestPulse_<version>.tar.gz)
  install      Install the package into your R library
  spell        Spell-check the package
  all          document + test + check  (default; the everyday loop)
"@
  }
}
