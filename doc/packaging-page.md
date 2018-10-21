---
author: Henrik Tramberend
date: '20.10.2018'
title: 'Decker Packaging [(DRAFT)]{style="color:red;"}'
---

# Executive summary

## Source Package

-   A ZIP archive containing the entire decker project directory
-   Needs to be locally compiled and installed
-   The name of the ZIP archive includes the full version number (with label) as the last component before the `.zip` extension
-   For developers only
-   All supported platforms

## Minimal Package

-   Just the decker executable
-   Needs to be installed somewhere in the PATH
-   The name of the decker executable includes the full version number (with label) as the last component (before the possible `.exe` extension on windows)
-   Only for command line use by command line savvy users
-   Executables are provided for all supported platforms

## macOS App Bundle

-   macOS only
-   For end users
-   Contains a GUI version of decker (See [`gui-architecture-page.md`](gui-architecture-page.md))
-   Distributed as App bundle on a downloadable DMG image

## Windows Installer

-   Windows only
-   For end users
-   Contains a GUI version of decker (See [`gui-architecture-page.md`](gui-architecture-page.md))
-   Distributed as a downloadable Windows installer MSI package

## Linux Package

-   Linux only
-   Distributed as a downloadable `.deb` package for Debian-based Linux distributions
-   For end users
-   Contains a GUI version of decker (See [`gui-architecture-page.md`](gui-architecture-page.md))
