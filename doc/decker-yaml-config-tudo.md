# Decker YAML Configuration: `tudo` Resource Pack

This document lists YAML keys defined or specifically used by the active `resource/tudo` pack. All core keys from `decker-yaml-config-decker.md` remain available.

## Defaults And Overrides

| Key | Shape | Usage |
| --- | --- | --- |
| `resource-pack: tudo` | string | Selects this pack from the in-tree resources, or equivalent URI/path selection. |
| `includeTULogo` | boolean | Controls display of the TU Dortmund logo on the title slide. Defaults to `true`. |
| `reveal` | map | Overrides the core Reveal profile: `center: false`, `overview: false`, `transition: none`, `width: 1280`, `height: 720`, PDF settings, and `checkOverflow: false`. |
| `explain.recWidth`, `explain.recHeight` | integers | Recording output dimensions for the explain plugin. |
| `explain.camWidth`, `explain.camHeight` | integers | Camera capture dimensions for explain. |
| `explain.useGreenScreen` | boolean | Enables/disables green-screen handling. |
| `chart.colors` | list of colors | Pack chart color list. |
| `chart.defaults` | map | Chart.js defaults using Lato and TU-oriented plugin settings. |
| `live-captions` | boolean | Defaults to `true`; includes live-captioning assets. |
| `caption-server` | URL | Defaults to `https://decker.cs.tu-dortmund.de/captions`. |
| `speech-recognition-language` | locale string | Defaults to `de-DE`. |
| `showDeckerLink` | boolean | Shows Decker link on title slide. Defaults to `true`. |
| `zoom.trigger` | string | Zoom plugin trigger, default `tripleClick`. |
| `quizzer.url` | URL | TU quiz server, default `https://quiz.jetzt/`. |
| `quizzer.audio.start`, `quizzer.audio.loop`, `quizzer.audio.end` | string | Audio cue settings for quizzer. |
| `quizzer.audio.volume` | number | Quizzer audio volume, default `0.2`. |
| `index.progress` | boolean | Enables progress on generated index. |
| `index.manual` | boolean | Shows manual link/section in `tudo` index templates. |
| `index.search` | boolean | Enables index search UI. |
| `index.searchShowsDeckTitle` | boolean | Search result display toggle. |
| `index.searchShowsDeckSubtitle` | boolean | Search result display toggle. |
| `index.listShowsDeckTitle` | boolean | Deck list display toggle. |
| `index.listShowsDeckSubtitle` | boolean | Deck list display toggle. |
| `index.links` | list | Defaults to `a11y`, `handout`, and `pdf`. |
| `palette.colors.light`, `palette.colors.dark` | list of 16 hex colors | TU-oriented light/dark palettes. |
| `palette.contrast` | number | Defaults to `0.40`. |
| `templates.deck` | template string | Defines the inline `:(deck)` macro output with HTML/PDF icons. |

Example skeleton:

```yaml
resource-pack: tudo

includeTULogo: true
showDeckerLink: true

reveal:
  width: 1280
  height: 720
  center: false
  overview: false
  transition: none
  progress: true
  pdfMaxPagesPerSlide: 10
  pdfSeparateFragments: false
  checkOverflow: false

explain:
  recWidth: 1920
  recHeight: 1080
  camWidth: 1280
  camHeight: 720
  useGreenScreen: false

chart:
  colors:
    - "#f5871f"
    - "#84B819"
    - "#2a9ddf"
  defaults:
    font:
      family: Lato
      size: 20

live-captions: true
caption-server: https://decker.cs.tu-dortmund.de/captions
speech-recognition-language: de-DE

zoom:
  trigger: tripleClick

quizzer:
  url: https://quiz.jetzt/
  audio:
    start: default
    loop: default
    end: default
    volume: 0.2

index:
  progress: true
  manual: true
  search: true
  searchShowsDeckTitle: true
  searchShowsDeckSubtitle: true
  listShowsDeckTitle: true
  listShowsDeckSubtitle: true
  links:
    - a11y
    - handout
    - pdf

palette:
  contrast: 0.40
  colors:
    light:
      - "#ffffff"
      - "#eeeeee"
      - "#d6d6d6"
      - "#8e908c"
      - "#969896"
      - "#4d4d4c"
      - "#282a2e"
      - "#1d1f21"
      - "#c82829"
      - "#f5871f"
      - "#eab700"
      - "#84B819"
      - "#3e999f"
      - "#2a9ddf"
      - "#8959a8"
      - "#a3685a"
    dark:
      - "#1d1f21"
      - "#282a2e"
      - "#373b41"
      - "#969896"
      - "#b4b7b4"
      - "#c5c8c6"
      - "#e0e0e0"
      - "#ffffff"
      - "#cc6666"
      - "#de935f"
      - "#f0c674"
      - "#7da72a"
      - "#8abeb7"
      - "#81a2be"
      - "#b294bb"
      - "#a3685a"

templates:
  deck: |
    <span class="icons">
      <a href=":(url)-deck.html">HTML</a>
      <a href=":(url)-deck.pdf">PDF</a>
    </span>
```

## Template Variables Used By `tudo`

| Key | Usage |
| --- | --- |
| `template.font` | Optional font stylesheet included by page/index/deck styles. |
| `template.css`, `template.page.css`, `template.deck.css`, `template.index.css` | CSS injection points. |
| `template.js`, `template.js-module`, `template.page.js`, `template.page.js-module`, `template.deck.js`, `template.deck.js-module` | Script injection points. |
| `template.favicon` | Favicon in generated pages. |
| `template.index-page.banner` | Banner image for the index/page header area. |
| `hide-quizzes` | Conditional deck flag used by the `tudo` deck template. |
| `watermark` | Optional deck watermark. |
| `logos`, `teaser`, `author`, `authors`, `affiliation`, `copyright`, `date`, `title`, `subtitle` | Title slide/page/index display metadata. |
| `index.manual`, `index.search`, `index.searchShowsDeckTitle`, `index.searchShowsDeckSubtitle`, `index.listShowsDeckTitle`, `index.listShowsDeckSubtitle` | `tudo` generated-index behavior toggles. |
| `decks.by-title`, `pages.by-title` | Generated index collections rendered by `index-generated.html`. |
| `chatty` | Conditional Chatty support in index templates. |
| `live-captions`, `reveal-plugins` | Conditional plugin script/style inclusion. |

Example skeleton:

```yaml
title: "TU Dortmund Deck"
subtitle: "Course unit"
author: "Ada Lovelace"
affiliation: "TU Dortmund"
date: "2026-09-22"

logos:
  - assets/tu-logo-light.svg
teaser:
  uri: assets/title-teaser.jpg
copyright:
  - type: cc-by
    url: https://creativecommons.org/licenses/by/4.0/

template:
  favicon: assets/favicon.ico
  font: assets/lato.css
  css:
    - assets/tudo-overrides.css
  page:
    css:
      - assets/tudo-page.css
    js-module:
      - assets/tudo-page.js
  deck:
    css:
      - assets/tudo-deck.css
    js-module:
      - assets/tudo-deck.js
  index:
    css:
      - assets/tudo-index.css
  index-page:
    banner: assets/index-banner.jpg

hide-quizzes: false
watermark: Draft

chatty:
  greeting: "How can I help?"

reveal-plugins:
  - name: CustomPlugin
    url: assets/custom-plugin.js
```

## Notes

`tudo` is more than a palette: it defines lecture-site index behavior, quizzer defaults, captions, explain recording defaults, and a title-slide/logo convention.
