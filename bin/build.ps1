<#
.SYNOPSIS
    Builds Decker for windows
.DESCRIPTION
    Build script to build Decker for windows. Allows to set the build
    mode via parameters for custom builds in the CI pipeline.
#>
Param(
    [switch] $skiptemplates,
    [switch] $preparepackage,
    [switch] $local
)

# Sets prefered action if error occurs to "Stop".
# Stops script execution even when non-terminating errors occur.
$ErrorActionPreference = "Stop"

<# Check if running as Administrator #>
$currentPrincipal = New-Object Security.Principal.WindowsPrincipal([Security.Principal.WindowsIdentity]::GetCurrent())
$admin = $currentPrincipal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)

if (-Not $admin -And $local) {
    Write-Error "You are not running as admin but using the -local parameter. Decker will not be copied to ${Env:ProgramFiles(x86)}"
}

$deckerdir = Split-Path $PSScriptRoot -Parent


Write-Host "Building Windows Decker" -ForegroundColor Green
if (-Not $skiptemplates) {
    Write-Output "Copying resources to resource directory is not available at the moment. See the Makefile for assistance."
}

<# Cleanup of old files #> 
Write-Host "Cleaning before new build" -ForegroundColor Green
& stack clean
if (test-path "$deckerdir\public"){
    Remove-Item "$deckerdir\public" -Recurse -Force -ErrorAction Ignore 
}


Write-Host "Starting build of standalone binary" -ForegroundColor Green

# Return to the decker root directory
Set-Location (Split-Path $PSScriptRoot -Parent)

Write-Host "Compiling the Haskell source code" -ForegroundColor Green
& stack build -j4



if ($preparepackage) {
    $binpath = (Join-Path ($(stack path | Select-String -Pattern "local-install-root") -split " ")[1] "bin\decker.exe")
    Copy-Item $binpath .

    $version = Get-Content .\package.yaml | Select-String -Pattern "version: "
    $version = $version -replace "\s+", " "
    $version = ($version -split " ")[1]
    Write-Output $version > version.txt
}

# If it's a local install, copy the program Decker to C:\ProgramFiles (x86)\Decker
# C:\ProgramFiles (x86)\Decker\bin needs to be on PATH to execute decker from anywhere
# needs Admin Rights
if ($local) {
    $exepath = (Join-Path ($(stack path | Select-String -Pattern "local-install-root") -split ": ")[1] "bin\decker.exe")

    $deckerpath = "${Env:ProgramFiles(x86)}\Decker"
    Write-Host "Copying the decker executable to $deckerpath\bin" -ForegroundColor Green

    
    New-Item -Path "$deckerpath\bin" -Force -ItemType "directory"

    Copy-Item "$exepath" "$deckerpath\bin\decker.exe"

    $version = Get-Content .\package.yaml | Select-String -Pattern "version: "
    $version = $version -replace "\s+", " "
    $version = ($version -split " ")[1]
    Write-Output $version > "$deckerpath\version.txt"
    $docs = [Environment]::GetFolderPath("MyDocuments")

    Write-Host "To then call decker from anywhere on the PowerShell command line create a PowerShell profile file, add the following line, and restart your PowerShell session!" -ForegroundColor Green
    Write-Host '$Env:Path += ";${Env:ProgramFiles(x86)}\Decker\bin"' -ForegroundColor Green
}