# Decker YAML Configuration: `mono` Resource Pack

This document lists YAML keys defined or specifically used by the active `resource/mono` pack. All core keys from `decker-yaml-config-decker.md` remain available.

## Defaults And Overrides

| Key | Shape | Usage |
| --- | --- | --- |
| `resource-pack: mono` | string | Selects this pack from the in-tree resources, or equivalent URI/path selection. |
| `poll-server` | URL | Defaults to `wss://tramberend.bht-berlin.de/quizzer/quiz`; local websocket alternative is commented in the pack. |
| `save-polls` | boolean | Defaults to `true`. |
| `experiments.fragment-templates` | boolean | Overrides core default to `true` for Codapi/live-code fragment template support. |
| `experiments.add-document-path` | boolean | Enables editor/source path annotation when not publishing lectures. |
| `palette.colors.light` | list of 16 hex colors | OKSolar-inspired light Base16 palette. |
| `palette.colors.dark` | list of 16 hex colors | OKSolar-inspired dark Base16 palette. |
| `palette.contrast` | number | Defaults to `0.3`. |
| `reveal` | map | Overrides core Reveal size and behavior: `width: 1920`, `height: 1080`, `backgroundTransition: slide`, `help: false`, and otherwise keeps most core defaults. |
| `css-variables.block-border-radius` | CSS value | Adds `--block-border-radius`, default `0.2em;`. |

Example skeleton:

```yaml
resource-pack: mono

poll-server: wss://tramberend.bht-berlin.de/quizzer/quiz
save-polls: true

experiments:
  fragment-templates: true
  add-document-path: true

palette:
  contrast: 0.3
  colors:
    light:
      - "#F3F2F1"
      - "#E6E5E1"
      - "#8faaab"
      - "#98a8a8"
      - "#657377"
      - "#5b7279"
      - "#212C37"
      - "#171F27"
      - "#dd459d"
      - "#f23749"
      - "#d56500"
      - "#ac8300"
      - "#819500"
      - "#259d94"
      - "#2b90d8"
      - "#7d80d1"
    dark:
      - "#171F27"
      - "#212C37"
      - "#5b7279"
      - "#657377"
      - "#98a8a8"
      - "#8faaab"
      - "#E6E5E1"
      - "#F3F2F1"
      - "#dd459d"
      - "#f23749"
      - "#d56500"
      - "#ac8300"
      - "#819500"
      - "#259d94"
      - "#2b90d8"
      - "#7d80d1"

reveal:
  width: 1920
  height: 1080
  backgroundTransition: slide
  help: false

css-variables:
  block-border-radius: 0.2em
```

## Template Variables Used By `mono`

The `mono` templates are close to core Decker and use the standard page/index/deck template keys.

| Key | Usage |
| --- | --- |
| `template.css`, `template.deck.css` | CSS injection into pages, index pages, and deck styles. |
| `template.js`, `template.js-module`, `template.page.js`, `template.page.js-module` | Page-level script injection. |
| `template.favicon` | Favicon in page/index templates. |
| `highlightjs`, `highlighting-css` | Syntax-highlighting stylesheet or Pandoc-generated highlighting CSS. |
| `live-captions`, `poll`, `reveal-plugins` | Conditional inclusion of supported plugin assets/scripts. |
| `chatty.index` | Optional index-page Chatty hook used by the `mono` index template. |
| `title`, `subtitle`, `author`, `authors`, `copyright`, `date`, `keywords`, `description-meta` | Standard visible and head metadata. |
| `css-light-color-declarations`, `css-dark-color-declarations`, `css-declarations` | Generated CSS variables emitted into templates. |

Example skeleton:

```yaml
title: "Mono Deck"
subtitle: "Compact visual preset"
author: "Ada Lovelace"
date: "2026-09-22"
keywords:
  - mono
description-meta: "A Decker deck using the mono resource pack."

template:
  favicon: assets/favicon.ico
  css:
    - assets/mono-overrides.css
  deck:
    css:
      - assets/mono-deck.css
  page:
    js-module:
      - assets/mono-page.js

highlightjs: xcode
live-captions: false
poll: false

reveal-plugins:
  - name: CustomPlugin
    url: assets/custom-plugin.js

chatty:
  index: true
```

## Notes

`mono` does not introduce a large custom metadata schema. It is mainly a visual/runtime preset: it changes Reveal dimensions, enables experimental features, sets poll defaults, and supplies a palette.
