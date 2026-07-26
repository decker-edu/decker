# Decker YAML Configuration: Core `decker`

This document describes YAML metadata keys defined by core Decker and keys that the core implementation reads or generates. It is based on `resource/decker/template/default.yaml` plus Haskell/template usage in `src/`, `app/`, and `resource/decker/template/`.

Resource-pack defaults are merged on top of these core defaults. Project `decker.yaml`, command-line `--meta`, and document front matter then override defaults.

## Core Defaults

| Key | Shape | Usage |
| --- | --- | --- |
| `experiments.fragment-templates` | boolean | Enables expansion of fragment template code blocks. Default is `false` in core. |
| `experiments.add-document-path` | boolean | When true and `lecture.publish` is not active, generated headings get editor/source path links. Not set by core default, but read by the Markdown reader. |
| `runtime-path-variables` | list of dotted keys | Metadata paths whose string values are treated as runtime resources and rewritten relative to the generated output. Core defaults include `logos`, `affiliation`, `author`, `authors`, `teaser`, `template`, and test keys under `rpv-test.*`. |
| `compiletime-path-variables` | list of dotted keys | Extra metadata paths that should be resolved at build time. Core always includes `csl`, `bibliography`, `meta-data`, `static-resource-dirs`, `static-resources`, and `extra-highlight-syntax`. |
| `chart.defaults` | map | Default Chart.js options merged into chart rendering. Core sets font size and plugin defaults. |
| `math.scale` | number | MathJax scaling factor used by deck rendering. |
| `math.macros` | map | MathJax macro definitions. Packs often extend this. |
| `index.progress` | boolean | Enables progress information on generated index pages. |
| `index.links` | list | Link types shown for each generated deck on index pages, for example `a11y`, `handout`, `pdf`, `presenter`. |
| `reveal` | map | Reveal.js initialization options. Core defaults include `width`, `height`, `controls`, `progress`, `slideNumber`, `transition`, `vertical-slides`, `viewDistance`, and related Reveal options. |
| `title` | string/boolean | Document/deck title. Core default is `false` so a title slide is omitted unless set. |
| `whisper.model` | path | Whisper model file used by transcription commands. |
| `whisper.lang` | string | Language passed to whisper transcription, default `de`. |
| `progress` | boolean | Legacy/global progress flag. Core sets `true`. |
| `lang` | string | Document language. Used in HTML `lang` attributes and quiz localization. Core default is `de`. |
| `editor.link-prefix` | string | URL prefix for editor links in generated source-path annotations. Core default is `zed://file`. |
| `dictionary` | map | Localized labels for exam and quiz UI. Core ships `de` and `en`. |
| `external-tools` | map | Tool definitions for filters/export/transcoding. Each tool has `command`, `arguments`, optional OS-specific overrides, optional `pipe`, and `help`. |

Example skeleton:

```yaml
experiments:
  fragment-templates: false
  add-document-path: false

runtime-path-variables:
  - logos
  - affiliation
  - author
  - authors
  - teaser
  - template

compiletime-path-variables:
  - assets/generated.css

chart:
  defaults:
    font:
      size: 20

math:
  scale: 1
  macros:
    R: '{{\mathrm{{I}\kern-.15em{R}}}}'

index:
  progress: true
  links:
    - a11y
    - handout

reveal:
  width: 1280
  height: 720
  transition: slide
  controls: true
  progress: true

title: "Deck Title"
lang: de

whisper:
  model: /opt/whisper/ggml-large-v3.bin
  lang: de

editor:
  link-prefix: zed://file
```

## Project And Build Keys

| Key | Shape | Usage |
| --- | --- | --- |
| `resource-pack` | URI/path string | Selects the resource pack. Examples: `mono`, `tudo`, `exe:mono`, a local directory, or a zip/source URI supported by Decker resources. Pack templates/defaults override core resources. |
| `meta-data` | list of paths | Additional YAML metadata files. Files are loaded recursively; later encountered metadata wins during the additional metadata merge. |
| `static-resource-dirs` | list of paths | Directories copied into `public/` as static resources and scanned as build inputs. |
| `static-resources` | list of paths | Additional individual static paths copied into `public/`. |
| `exclude-directories` | list of paths | Extra directories excluded from target scanning. Core always excludes `public`, `chatty`, `dist`, `.git`, `.vscode`, and `.stack-work`. |
| `watch.exclude` | list of globs | Glob patterns excluded from watch mode. |
| `decker-version` | string | Optional project compatibility/version metadata checked by the version module. |
| `write-back.enable` | boolean | Enables writing parsed/normalized Markdown back to the source file. Can be set globally or in document metadata. |
| `write-back.line-columns` | integer | Column target for write-back formatting, default `80`. |
| `write-back.line-wrap` | string | Pandoc line wrap mode for write-back, default `auto`; tests cover values such as `none`. |
| `publish.rsync.destination` | string | Required by `decker publish` when rsync publishing is used. |
| `publish.rsync.options` | list | Merge-replaced option list for rsync publishing. |

Example skeleton:

```yaml
resource-pack: mono

meta-data:
  - shared/course-meta.yaml

static-resource-dirs:
  - assets

static-resources:
  - favicon.ico

exclude-directories:
  - archive

watch:
  exclude:
    - "public/**"
    - "tmp/**"

decker-version: 0.14.0

write-back:
  enable: false
  line-columns: 80
  line-wrap: auto

publish:
  rsync:
    destination: user@example.org:/var/www/course/
    options:
      - --recursive
      - --delete
```

## Rendering And Template Keys

| Key | Shape | Usage |
| --- | --- | --- |
| `palette.colors.light` | list of 16 hex colors | Base16-like light palette. Used to derive CSS variables `base00`..`base0F`, `shade0`.., and `accent0`... |
| `palette.colors.dark` | list of 16 hex colors | Base16-like dark palette. If omitted, light colors are reused; if both are omitted, core built-ins are used. |
| `palette.contrast` | number | Percentage used to derive the `--contrast` CSS variable. Default fallback in code is `0.25`; core default sets no palette but pack defaults usually do. |
| `css-variables` | map of CSS variable names to values | Converted to declarations such as `--font-size-base: 28px;` and emitted into templates. Values are inserted as CSS text. |
| `css-light-colors` | map | Manual light color variable overrides. These are left-biased over derived palette colors. |
| `css-dark-colors` | map | Manual dark color variable overrides. These are left-biased over derived palette colors. |
| `highlightjs` | string | Name of a bundled Highlight.js CSS theme. When set, Pandoc's own highlight style is disabled and templates include the selected CSS file. |
| `highlight-style` | string | Pandoc/skylighting style name used when `highlightjs` is not set. Fallback is `monochrome`. |
| `extra-highlight-syntax` | map | Additional Skylighting syntax definitions, keyed by syntax name and valued by file path. |
| `mathjax-url` | string | MathJax URL passed to Pandoc writer options. Empty string means the template/runtime handles MathJax. |
| `template.favicon` | path | Favicon link included by page/index/deck templates. |
| `template.css` | list/path | Global CSS files included by page/index/handout/deck styles templates. |
| `template.js` | list/path | Global classic or module scripts included by templates, depending on template. |
| `template.js-module` | list/path | Global module scripts. |
| `template.page.css` | list/path | Page-only CSS, used by packs that support it. |
| `template.page.js` | list/path | Page/index-only classic scripts. |
| `template.page.js-module` | list/path | Page/index-only module scripts. |
| `template.deck.css` | list/path | Deck-only CSS. |
| `template.deck.js` | list/path | Deck-only classic scripts. |
| `template.deck.js-module` | list/path | Deck-only module scripts. |
| `templates.*` | string template | Inline content templates expanded by the `:(name)` template macro filter. Example: `templates.deck`. |

Example skeleton:

```yaml
palette:
  contrast: 0.25
  colors:
    light:
      - "#ffffff"
      - "#e0e0e0"
      - "#d6d6d6"
      - "#8e908c"
      - "#969896"
      - "#4d4d4c"
      - "#282a2e"
      - "#1d1f21"
      - "#c82829"
      - "#f5871f"
      - "#eab700"
      - "#718c00"
      - "#3e999f"
      - "#4271ae"
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
      - "#b5bd68"
      - "#8abeb7"
      - "#81a2be"
      - "#b294bb"
      - "#a3685a"

css-variables:
  font-size-base: 28px
  vertical-margin: 0.7rem

css-light-colors:
  accent0: "#c82829"

css-dark-colors:
  accent0: "#cc6666"

highlightjs: xcode
highlight-style: monochrome
mathjax-url: ""

extra-highlight-syntax:
  my-language: syntax/my-language.xml

template:
  favicon: assets/favicon.ico
  css:
    - assets/global.css
  js-module:
    - assets/global.js
  page:
    css:
      - assets/page.css
  deck:
    css:
      - assets/deck.css
    js-module:
      - assets/deck.js

templates:
  deck: |
    <a href=":(url)-deck.html">Slides</a>
```

## Standard Document Metadata Used By Core Templates

These are mostly Pandoc metadata keys, but core templates and filters depend on them.

| Key | Shape | Usage |
| --- | --- | --- |
| `title`, `subtitle`, `date`, `author`, `authors`, `affiliation`, `copyright`, `logos`, `teaser` | scalar/list/map | Title slide, page header, generated index cards, and metadata tags. Runtime path resolution can rewrite asset paths in several of these keys. |
| `title-prefix`, `pagetitle`, `description-meta`, `date-meta`, `author-meta`, `keywords`, `dir` | scalar/list | HTML head metadata and document title generation. Some are Pandoc-derived writer variables. |
| `header-includes`, `include-before`, `include-after` | raw blocks/list | Injected into HTML templates. |
| `bibliography`, `csl`, `reference-location` | path/string | Citation processing. If `bibliography` is set and `csl` is absent, Decker installs its default CSL from resources. |
| `toc`, `table-of-contents`, `toc-title` | boolean/string | Table of contents support in page-like templates. |
| `watermark` | string/raw HTML | Optional deck watermark slot. |
| `showDeckerLink` | boolean | Controls whether the Decker link is shown on supported title slides. |

Example skeleton:

```yaml
title: "Presentation Title"
subtitle: "Optional Subtitle"
date: "2026-09-22"
author:
  - name: "Ada Lovelace"
    affiliation: "Example University"
authors:
  - name: "Ada Lovelace"
affiliation:
  - name: "Example University"
    logo: assets/university-logo.svg
copyright:
  - type: cc-by
    url: https://creativecommons.org/licenses/by/4.0/
logos:
  - assets/logo.svg
teaser:
  uri: assets/teaser.jpg
  alt: "Teaser image"

keywords:
  - decker
  - slides
description-meta: "Short description for HTML metadata."
dir: ltr

header-includes:
  - |
    <meta name="theme-color" content="#ffffff">
include-before:
  - |
    <div class="before-content"></div>
include-after:
  - |
    <script src="assets/after.js"></script>

bibliography: bibliography.bib
csl: chicago-author-date.csl
reference-location: block

toc: true
toc-title: Contents
watermark: Draft
showDeckerLink: true
```

## Feature And Plugin Keys

| Key | Shape | Usage |
| --- | --- | --- |
| `poll` | map/list | Poll definitions parsed by the poll filter. Per-poll keys include `color`, `timed`, `seconds`, `font-color`, `font-size`, and `font-style`. |
| `poll-server` | URL | Quizzer/poll server URL used by poll-related plugins. |
| `save-polls` | boolean | Enables saving poll results where supported by the plugin/runtime. |
| `quiz.style` | string | Default quiz rendering style, usually `fancy` or `plain`. |
| `quiz.solution` | string | Default quiz solution class/visibility marker. |
| `score`, `category`, `lectureId`, `topic`, `lang` | scalar | Per-quiz YAML block keys consumed by the quiz filter. |
| `lecture.publish` | boolean/string | Controls publishing behavior and source-path annotation. The select filter also checks textual `yes`. |
| `lecture.status` | string | Values such as `draft`, `upcoming`, and `done` are used by index and select/publish filters. |
| `draft` | boolean | Excludes a target from generated index information. |
| `no-index` | list | Target URLs excluded from generated index output. |
| `feedback.deck-id` | string | Used by index generation and feedback plugin wiring. |
| `short-links` | map | Defines short URI bindings. Lookup supports direct schemes, `short-links.bind.<scheme>`, and nested scheme bindings. |
| `reveal-plugins` | list of maps | Adds custom Reveal plugin scripts. Templates expect entries with `name` and `url`. |
| `live-captions` | boolean | Includes live-captioning assets in supported templates. |
| `caption-server` | URL | Live caption service endpoint. |
| `speech-recognition-language` | string | Browser speech recognition locale, for example `de-DE`. |
| `codapi.server` | URL | Enables Codapi snippet runtime server setup in supported templates. |
| `chatty.prompt` | string | OpenAI prompt id used by Chatty sealing. |
| `chatty.instructions` | string/path | Plain instructions or a path read during sealing. Deleted from public metadata after sealing. |
| `chatty.model` | string | Model name sealed into Chatty config; default in code is `gpt-4.1`. |
| `chatty.params` | map | JSON parameters sealed into Chatty config. |
| `chatty.vector-store-id` | string | Existing vector store id for Chatty upload/sync; if absent, upload may create one. |
| `chatty.vector-store-name` | string | Name used when creating a vector store; default is `decker`. |
| `chatty.extra` | list of directories | Extra directories uploaded into the Chatty vector store. |
| `chatty.greeting` | string | Runtime greeting fallback used by Chatty UI where present. |

Example skeleton:

```yaml
poll-server: wss://example.org/quizzer/quiz
save-polls: true

poll:
  color: "#008cff"
  timed: true
  seconds: "30"
  font-color: "#000"
  font-size: 18
  font-style: bold

quiz:
  style: fancy
  solution: solution-on-demand

lecture:
  publish: false
  status: upcoming

draft: false
no-index:
  - private-deck.html

feedback:
  deck-id: course-week-01

short-links:
  bind:
    issue: github
  github:
    decker: https://github.com/decker-edu/decker

reveal-plugins:
  - name: ExamplePlugin
    url: assets/example-plugin.js

live-captions: true
caption-server: https://example.org/captions
speech-recognition-language: de-DE

codapi:
  server: https://codapi.example.org

chatty:
  prompt: pmpt_...
  instructions: prompts/course-instructions.md
  model: gpt-4.1
  params:
    temperature: 0.2
  vector-store-id: vs_...
  vector-store-name: course-materials
  extra:
    - chatty-extra
  greeting: "How can I help with this course?"
```

## Generated Metadata

These keys are written by Decker and are useful in templates, but should normally not be authored manually.

| Key | Usage |
| --- | --- |
| `build-time` | Timestamp added when global metadata is read. |
| `decker-meta-url` | Relative JSON metadata file emitted next to generated HTML. |
| `decker-support-dir` | Relative URL path from current output to `support/`. |
| `decker.base-dir`, `decker.doc-path` | Absolute/current document paths used by filters. |
| `decker.filter.resources`, `decker.filter.links` | Resource/link dependencies collected by filters. |
| `decker-meta` | JSON dump of metadata embedded by the meta filter. |
| `css-declarations`, `css-light-color-declarations`, `css-dark-color-declarations` | CSS declaration lists derived from `css-variables` and palettes. |
| `css-light-colors`, `css-dark-colors` | Derived color maps available to templates. |
| `targets` | Build target map added for commands that render decks/pages/indexes. |
| `decks.by-title`, `decks.by-date`, `decks.by-url`, `decks.by-author`, `decks.by-id` | Generated index deck collections. |
| `pages.by-title`, `pages.by-date`, `pages.by-url`, `pages.by-author`, `pages.by-id` | Generated index page collections. |
| `quests.by-title`, `quests.by-url`, `quests.by-lecture-id`, `quests.by-topic-id` | Generated quest collections. |
| `chatty.filepath`, `chatty.url-path`, `chatty.included-from` | Internal Chatty source annotations. |
| `chatty.sealed-config` | Public-safe sealed Chatty config blob. |

Generated shape:

```yaml
build-time: "22.07.2026 15:30"
decker-meta-url: 123456789.json
decker-support-dir: support

decker:
  base-dir: /absolute/path/to/source
  doc-path: /absolute/path/to/source/example-deck.md
  filter:
    resources:
      - assets/image.png
    links:
      - https://example.org

css-declarations:
  - "--font-size-base: 28px;"
css-light-color-declarations:
  - "--base00: #ffffff;"
css-dark-color-declarations:
  - "--base00: #1d1f21;"

targets:
  sources:
    - example-deck.md
  decks:
    public/example-deck.html: example-deck.md

decks:
  by-title: []
pages:
  by-title: []
quests:
  by-title: []

chatty:
  filepath: /absolute/path/to/source/example-deck.md
  url-path: /absolute/path/to/public/example-deck.html
  sealed-config: sealed-public-blob
```
