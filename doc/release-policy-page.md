---
author: Henrik Tramberend
date: '20.10.2018'
title: 'Decker Release Process [(DRAFT)]{style="color:red;"}'
---

# Executive summary

## Versioning

Decker follows [*Semantic Versioning*](https://semver.org).

Semantic Versioning in a nutshell (from [semver.org](https://semver.org)):

> Given a version number MAJOR.MINOR.PATCH, increment the:
>
> -   MAJOR version when you make incompatible API changes,
> -   MINOR version when you add functionality in a backwards-compatible manner, and
> -   PATCH version when you make backwards-compatible bug fixes.
>
> Additional labels for pre-release and build metadata are available as extensions to the MAJOR.MINOR.PATCH format.

## Release Process

-   Backwards-compatible bug fixes are made on the corresponding release branch
-   Bug fix releases are cut from the *release branch* on which the bug was fixed
-   Bug fixes are merged back to the master branch to be included in the next regular release
-   Features are developed on their own *feature branches*
-   Releases are cut from the master branch
-   For each release a *release branch* is created (named after the version number) immediately upon release
-   After each release the MINOR version number on the master branch is incremented
-   For each release a release plan consisting of the list of included features is made
-   Backwards-compatible features slated for the next MINOR release are merged onto the *master branch*
-   Non backwards-compatible features slated for the next MAJOR release are merged onto a *major release branch*
-   Features *not* slated for the next MAJOR or MINOR release are *not* merged onto any branch
-   For a release with backwards-compatible features and bug fixes only the MINOR version is incremented
-   For a release with *non* backwards-compatible features or bug fixes the MAJOR version is incremented
-   Pre-releases are cut from an appropriately named pre-release branch where the desired pre-release features are merged
-   Pre-release branches and versions are labeled with a descriptive name (i.e. `1.3.0-some-important-demo-or-other`)
-   The current version number is maintained in `package.yaml` and only there
-   Feature and release branches are not deleted after merge
