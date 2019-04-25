<#
.SYNOPSIS
    Builds Decker for windows
.DESCRIPTION
    Build script to build Decker for windows. Allows to set the build
    mode via parameters for custom builds in the CI pipeline.
.PARAMETER buildtype
    Defaults to assume preextracted resources for the build.
    If set to any other value will include the resources in the executable.
#>
Param(
    [string] $buildtype = "preextracted",
    [switch] $skiptemplates,
    [switch] $preparepackage
)
Write-Output "Building Windows Decker"
if(-Not $skiptemplates) {
    & yarn install
    & yarn run webpack --mode production
    Copy-Item -Recurse -Force node_modules/reveal.js-menu resource/support/
    New-Item -ItemType directory -Force -Path resource/support/notes
    Copy-Item -Force node_modules/reveal.js/plugin/notes/notes.html resource/support/notes/notes.html
    Copy-Item -Force node_modules/reveal.js/plugin/notes/notes.js resource/support/notes/notes.js
    New-Item -ItemType directory -Force -Path resource/support/print
    Copy-Item -Force node_modules/reveal.js/css/print/paper.css resource/support/print/paper.css
    Copy-Item -Force node_modules/reveal.js/css/print/pdf.css resource/support/print/pdf.css
}

if($buildtype -eq "preextracted"){
    Write-Output "Building for preextracted resources"
    & stack build -j4 --flag decker:preextractedresources
} else {
    Write-Output "Building standalone binary"
    & stack build -j4
}

if($preparepackage) {
    $binpath=(Join-Path ($(stack path | Select-String -Pattern "local-install-root") -split " ")[1] "bin\decker.exe")
    Copy-Item $binpath .

    $version = Get-Content .\package.yaml | Select-String -Pattern "version: "
    $version = $version -replace "\s+", " "
    $version = ($version -split " ")[1]
    Write-Output $version > version.txt

    Compress-Archive -Force -Path .\resource -CompressionLevel Fastest -DestinationPath resource
}