@echo off
REM Wrapper so dev.ps1 runs regardless of PowerShell execution policy.
REM Usage:  dev.cmd <task>     e.g.  dev.cmd build
powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0dev.ps1" %*
