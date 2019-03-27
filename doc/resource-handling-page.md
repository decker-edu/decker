---
author: Henrik Tramberend
date: '20.10.2018'
title: 'Resource Handling [(DRAFT)]{style="color:red;"}'
---

# Executive summary

## What are resources?

-   All local data files that are required for proper operation at *decker-run-time* or *deck-presentation-time*
-   In particular these are (as of now)
    1.  Pandoc template files in `resource/template` (decker-run-time)
    2.  HTML support files in `resource/support` (deck-presentation-time)
    3.  Example presentation source files in `resource/example` (decker-run-time)
-   Resources are highly specific to the decker version that uses them

## Resource Bundles

-   Template, support and example resources are combined into a *resource bundle*
-   A resource bundle may contain a `resource.yaml` file with meta information regarding bundle author etc.
-   The data from `resource.yaml` is available in the meta data during slide compilation under some key like `resource-meta` or such
-   A resource bundle is a compressed ZIP archive of the contents of the `resource` folder
-   Resource bundles are located at run-time via their URL
-   Three protocol schemes are supported
    -   `decker:` the *default resource bundle* that is located in the currently running executable (ie. `decker:decker-resource-1.3.0.zip`)
    -   `file:` the resource bundle is located in the local file system (ie. `file://home/henrik/decker-personal-resource-1.3.0.zip`)\
    -   `https:` the resource bundle is located on a remote server (ie. `https://decker.uni-wü.de/decker-hci-style-resource-1.3.0.zip`)\
    -   `dev:` this is special: the bundle is located in unpacked state in the project directory during development (this is used when decker is executed with `stack exec -- decker`)
-   The packed resource bundle is unpacked into a local cache directory on first use of decker
-   If localization, acquisition, unpacking or caching fails decker terminates
-   The local cache can be cleared with `decker clear-cache`
-   The URL of the resource bundle can be specified at runtime in the meta data
-   Resource bundles (with the exception of the `dev:` bundle) must never be changed
-   Decker supports localization, acquisition, unpacking and caching of resource bundles without the help of external programs on all supported platforms
-   The decker executable always contains the default resource bundle that is used when no resource bundle is specified in the meta data
-   Template Haskell (`file-embed`) is no longer used to embed resources

## Versioning

-   Resource bundles are always tied to a specific decker version
-   The exact decker version (MAJOR.MINOR.PATCH-LABEL) is always the last component of the resource file name
-   If decker is used with a non-matching resource bundle decker is terminated
    -   This can be down-graded to a warning with a meta data setting
