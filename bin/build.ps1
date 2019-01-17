Param(
    [string]$buildtype = "preextracted"
)
Write-Output "Building Windows Decker"
& yarn install
& yarn run webpack --mode production
Copy-Item -Recurse -Force node_modules/reveal.js-menu resource/support/
New-Item -ItemType directory -Force -Path resource/support/notes
Copy-Item -Force node_modules/reveal.js/plugin/notes/notes.html resource/support/notes/notes.html
Copy-Item -Force node_modules/reveal.js/plugin/notes/notes.js resource/support/notes/notes.js
New-Item -ItemType directory -Force -Path resource/support/print
Copy-Item -Force node_modules/reveal.js/css/print/paper.css resource/support/print/paper.css
Copy-Item -Force node_modules/reveal.js/css/print/pdf.css resource/support/print/pdf.css

if($buildtype -eq "preextracted"){
    Write-Output "Building for preextracted resources"
    & stack build -j4 --flag decker:preextractedresources
} else {
    Write-Output "Building standalone binary"
    & stack build -j4
}

