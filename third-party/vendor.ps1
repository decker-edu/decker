<#
.SYNOPSIS
    Builds and copies vendor libraries to the support folder
.DESCRIPTION
    Counterpart to symlinks.mk for Windows. As symlinks are not properly 
    supported, all required files are copied.
#>

$third = Split-Path -parent $PSCommandPath
$decker = Split-Path $third -parent
$support = $ExecutionContext.SessionState.Path.GetUnresolvedProviderPathFromPSPath("$decker\resource\support\vendor")

Write-Host "Building third party dependencies" -ForegroundColor Green

# Build jquery
Set-Location "$third\jquery"
& npm run build

# Build thebelab
Set-Location "$third\thebelab"
& npm install
& npm run build

# Build Chart.js
Set-Location "$third\Chart.js"
& npm install
& npx rollup -c rollup.config.js

Write-Host ("Copy third party dependencies to " + $support) -ForegroundColor Green
New-Item -Path $support -Force -ItemType "directory"
Set-Location $third

# Copy jquery
Copy-Item "$third\jquery\dist\jquery.min.js" "$support\jquery.js -Force"

# Copy thebelab
New-Item -Path "$support\thebelab" -Force -ItemType "directory"
Copy-Item $third\thebelab\lib\*.js "$support\thebelab" -Force
Copy-Item $third\thebelab\lib\*.map "$support\thebelab" -Force

# Copy mathjax
New-Item -Path "$support\mathjax\jax\input" -Force -ItemType "directory"
New-Item -Path "$support\mathjax\jax\output" -Force -ItemType "directory"
Foreach ($i in ("MathJax.js", "config", "jax\input\TeX", "jax\output\SVG", "jax\element", "extensions")) {
  Copy-Item -Recurse "$third\MathJax\$i" "$support\mathjax\$i" -Force
}

# Copy reveal.js
New-Item "$support\reveal\plugin" -Force -ItemType "directory"
New-Item "$support\reveal\plugin\markdown" -Force -ItemType "directory"
New-Item "$support\reveal\plugin\markdown" -Force -ItemType "directory"
Copy-Item "$third\reveal.js\plugin\markdown\marked.js" "$support\reveal\plugin\markdown\marked.js" -Force
Foreach ($i in ("js", "css", "lib", "plugin\math", "plugin\zoom-js", "plugin\notes")) {
  Copy-Item -r "$third\reveal.js\$i" "$support\reveal\$i" -Force
}

# Copy Reveal.js-menu
New-Item "$support\reveal.js-menu" -Force -ItemType "directory"
Foreach ($i in ("menu.css", "menu.js")) {
  Copy-Item -r "$third\reveal.js-menu\$i" "$support\reveal.js-menu\$i" -Force
}

# Copy bootstrap
New-Item "$support\bootstrap" -Force -ItemType "directory"
Copy-Item -Recurse "$third\bootstrap\dist\css" "$support\bootstrap\css" -Force

# Copy piklor.js
Copy-Item -Recurse "$third\piklor.js\src\piklor.min.js" "$support\piklor.js" -Force

# Copy whiteboard
Copy-Item -Recurse "$third\mb-reveal-plugins\whiteboard" "$support\whiteboard" -Force

# Copy Charts
Copy-Item -Recurse "$third\mb-reveal-plugins\charts" "$support\charts" -Force

# Copy math
Copy-Item -Recurse "$third\mb-reveal-plugins\math" "$support\math" -Force

# Copy highlight
Copy-Item -Recurse "$third\mb-reveal-plugins\highlight" "$support\highlight" -Force

# Copy fontawesome
New-Item "$support\fontawesome" -Force -ItemType "directory"
Foreach ($i in ( "js", "css", "webfonts", "svgs", "sprites")) {
  Copy-Item -Recurse "$third\Font-Awesome\js-packages\@fortawesome\fontawesome-free\$i" "$support\fontawesome\$i" -Force
}
