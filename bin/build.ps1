Write-Output "Building Windows Decker"
& yarn install
& yarn run webpack --mode production
Copy-Item -Recurse -Force node_modules/reveal.js-menu resource/support/
#Copy-Item -Recurse -Force node_modules/reveal.js/plugin/notes resource/support/
New-Item -ItemType directory -Force -Path resource/support/notes
Copy-Item -Force node_modules/reveal.js/plugin/notes/notes.html resource/support/notes/notes.html
Copy-Item -Force node_modules/reveal.js/plugin/notes/notes.js resource/support/notes/notes.js
New-Item -ItemType directory -Force -Path resource/support/print
Copy-Item -Force node_modules/reveal.js/css/print/paper.css resource/support/print/paper.css
Copy-Item -Force node_modules/reveal.js/css/print/pdf.css resource/support/print/pdf.css
& stack build -j4

$binpath=(Join-Path ($(stack path | Select-String -Pattern "local-install-root") -split " ")[1] "bin\decker.exe")
Copy-Item $binpath .

$version = Get-Content .\package.yaml | Select-String -Pattern "version: "
$version = $version -replace "\s+", " "
$version = ($version -split " ")[1]
Write-Output $version > version.txt

Compress-Archive -Force -Path .\resource -CompressionLevel Fastest -DestinationPath resource
