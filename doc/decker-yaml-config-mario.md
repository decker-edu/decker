# Decker YAML Configuration: `mario` Resource Pack

This document lists YAML keys defined or specifically used by the active `resource/mario` pack. All core keys from `decker-yaml-config-decker.md` remain available.

## Defaults And Overrides

| Key | Shape | Usage |
| --- | --- | --- |
| `resource-pack: mario` | string | Selects this pack from the in-tree resources, or equivalent URI/path selection. |
| `reveal` | map | Overrides core Reveal setup: `center: false`, `overview: false`, `progress: false`, `transition: none`, `width: 1280`, `height: 720`, PDF settings, and other standard Reveal switches. |
| `chart.defaults` | map | Chart.js defaults using Lato and `tableau.Classic10`. |
| `math.scale` | number | Overrides core math scale to `0.9`. |
| `math.macros` | map | Adds a large math macro set, including `R`, `laplace`, `grad`, `abs`, `norm`, `vec`, `mat`, `diff`, `pdiff`, and related aliases. |
| `highlightjs` | string | Defaults to `xcode`. |
| `palette.colors.light`, `palette.colors.dark` | list of 16 hex colors | Mario pack palettes. |

Example skeleton:

```yaml
resource-pack: mario

reveal:
  width: 1280
  height: 720
  center: false
  overview: false
  progress: false
  transition: none
  pdfMaxPagesPerSlide: 10
  pdfSeparateFragments: false

chart:
  defaults:
    font:
      family: Lato
      size: 20
    plugins:
      colorschemes:
        scheme: tableau.Classic10

math:
  scale: 0.9
  macros:
    R: '{{\mathrm{{I}\kern-.15em{R}}}}'
    laplace: '{\Delta}'
    grad: '{\nabla}'
    vec:
      - '{\mathbf{\boldsymbol{#1}}}'
      - 1

highlightjs: xcode

palette:
  colors:
    light:
      - "#ffffff"
      - "#e8e8e8"
      - "#d8d8d8"
      - "#b8b8b8"
      - "#585858"
      - "#383838"
      - "#282828"
      - "#181818"
      - "#ff0000"
      - "#d89326"
      - "#ffff00"
      - "#83B818"
      - "#00ffa5"
      - "#2a9ddf"
      - "#a500ff"
      - "#ff00ff"
    dark:
      - "#181818"
      - "#282828"
      - "#383838"
      - "#585858"
      - "#b8b8b8"
      - "#d8d8d8"
      - "#e8e8e8"
      - "#f8f8f8"
      - "#ab4642"
      - "#dc9656"
      - "#f7ca88"
      - "#a1b56c"
      - "#86c1b9"
      - "#7cafc2"
      - "#ba8baf"
      - "#a16946"
```

## Template Variables Used By `mario`

| Key | Usage |
| --- | --- |
| `template.base-css` | Optional base stylesheet for Mario page templates. |
| `template.css` | Additional CSS included in page/index/deck templates. |
| `template.favicon` | Favicon in generated pages. |
| `template.index-page.banner` | Banner image in page/index layouts. |
| `template.teaser` | Teaser image used by the Mario deck title area. |
| `template.page.js`, `template.page.js-module` | Page/index script injection. |
| `toc`, `table-of-contents`, `toc-title`, `idprefix` | Page/index/deck table-of-contents and id controls inherited from Pandoc template conventions. |
| `decker-meta` | Embedded metadata dump consumed by the Mario page template. |
| `title`, `subtitle`, `author`, `authors`, `affiliation`, `copyright`, `date` | Title and deck/page display metadata. |
| `css-light-color-declarations`, `css-dark-color-declarations`, `css-declarations` | Generated CSS declarations emitted into templates. |
| `highlightjs`, `highlighting-css`, `quotes` | Syntax and Pandoc style slots. |

Example skeleton:

```yaml
title: "Mario Deck"
subtitle: "Template-specific deck"
author: "Ada Lovelace"
authors:
  - name: "Ada Lovelace"
    affiliation: "Example University"
affiliation: "Example University"
copyright: "CC BY 4.0"
date: "2026-09-22"

template:
  favicon: assets/favicon.ico
  base-css: assets/mario-base.css
  css:
    - assets/mario-overrides.css
  index-page:
    banner: assets/index-banner.jpg
  teaser: assets/title-teaser.jpg
  page:
    js-module:
      - assets/mario-page.js

toc: true
table-of-contents: true
toc-title: Contents
idprefix: mario-

quotes: true
highlightjs: xcode
```

## Notes

`mario` has a custom deck/page template family and a substantial math macro preset, but it does not define the richer generated-index schema used by `jmu-hci-drp`.
