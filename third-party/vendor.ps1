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

# Build thebelab
Set-Location "$third\thebelab"
& npm install
& npm run build

Write-Host ("Copy third party dependencies to " + $support) -ForegroundColor Green
New-Item -Path "$support" -Force -ItemType "directory"
Set-Location $third

# Copy thebelab
New-Item -Path "$support\thebelab" -Force -ItemType "directory"
Copy-Item $third\thebelab\lib\*.js "$support\thebelab" -Force
# Copy-Item $third\thebelab\lib\*.map "$support\thebelab" -Force

# Copy mathjax
New-Item -Path "$support\mathjax\input" -Force -ItemType "directory"
New-Item -Path "$support\mathjax\output" -Force -ItemType "directory"
Foreach ($i in ("tex-svg.js", "input\tex", "input\tex.js". "output\svg", "output\svg.js")) {
  Copy-Item -Recurse "$third\MathJax\es5\$i" "$support\mathjax\$i" -Force
}

# Copy reveal.js
New-Item "$support\reveal\" -Force -ItemType "directory"
Copy-Item -r "$third\reveal.js\dist" "$support\reveal\" -Force
Copy-Item -r "$third\reveal.js\plugin" "$support\reveal\" -Force

# Copy water.css
New-Item "$support\css" -Force -ItemType "directory"
Copy-Item "$third\water.css\dist\light.min.css" "$support\css\light.min.css"

# Copy styles for highlight.js
New-Item "$support\css" -Force -ItemType "directory"
Copy-Item "$third\highlight.js\*.css" "$support\css\"

# Copy fontawesome
New-Item "$support\fontawesome\css" -Force -ItemType "directory"
# New-Item "$support\fontawesome\webfonts" -Force -ItemType "directory"
Foreach ($i in ( "css\all.css", "webfonts")) {
  Copy-Item -Recurse "$third\Font-Awesome\js-packages\@fortawesome\fontawesome-free\$i" "$support\fontawesome\$i" -Force
}

# Copy video.js
New-Item -Path "$support\videojs" -Force -ItemType "directory"
Copy-Item -Recurse "$third\video.js\*" "$support\videojs" -Force
