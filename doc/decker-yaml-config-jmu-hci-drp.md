# Decker YAML Configuration: `jmu-hci-drp` Resource Pack

This document lists YAML keys defined or specifically used by the external `jmu-hci-drp` pack at `/Users/marc/repositories/development/jmu-hci-drp`. This pack is intended as the replacement for deprecated `wburg` and `nwburg`; those deprecated packs were not analyzed here.

All core keys from `decker-yaml-config-decker.md` remain available.

## Selecting The Pack

| Key | Shape | Usage |
| --- | --- | --- |
| `resource-pack` | path/URI string | The pack README shows local usage such as `resource-pack: jmu-hci-drp` or `resource-pack: my-resource-packs/jmu-hci-drp`. In a Decker development tree, an executable resource URI can also be used if packaged. |

Example skeleton:

```yaml
resource-pack: jmu-hci-drp

static-resource-dirs:
  - assets
```

## Defaults And Overrides

| Key | Shape | Usage |
| --- | --- | --- |
| `reveal` | map | JMU-oriented Reveal defaults: `width: 1244`, `height: 700`, `center: false`, `transition: none`, `menu: true`, `zoom: true`, and standard Reveal switches. |
| `palette.colors.light`, `palette.colors.dark` | list of 16 hex colors | JMU/HCI palette used to derive CSS variables. |
| `palette.contrast` | number | Defaults to `0.3`. |
| `css-variables.font-family` | CSS value | Base font family, default `Roboto`. |
| `css-variables.font-size-base` | CSS length | Base deck font size, default `28px`. |
| `css-variables.inactive-color` | CSS color | Used by support CSS for inactive UI states. |
| `css-variables.icon-size` | CSS length | Icon size, default `2vmin`. |
| `css-variables.margin-columns` | CSS length | Column margin variable. |
| `css-variables.block-border-width` | CSS length | Block border width. |
| `css-variables.block-border-padding` | CSS length | Block padding. |
| `css-variables.block-border-style` | CSS keyword | Block border style. |
| `css-variables.vertical-margin` | CSS length | Vertical spacing variable. |
| `template.title-page.vertical-margin` | CSS length | Title page spacing; default `1em`. |
| `template.title-page.teaser-img.height` | CSS/image length | Default teaser image height, default `200px`. |
| `template.title-page.teaser-img.margin` | CSS length | Top/bottom teaser margin, default `50px`. |
| `template.title-page.affiliation-logo.height` | CSS/image length | Default affiliation logo height, default `70px`. |
| `math.scale` | number | Defaults to `0.9`. |
| `math.macros` | map | MathJax macro preset inherited from the old HCI/Wuerzburg style. |

Example skeleton:

```yaml
reveal:
  width: 1244
  height: 700
  center: false
  transition: none
  menu: true
  zoom: true

palette:
  contrast: 0.3
  colors:
    light:
      - "#ffffff"
      - "#EDEDED"
      - "#DADADA"
      - "#C5C5C5"
      - "#AEAEAE"
      - "#919191"
      - "#6A6A6A"
      - "#000000"
      - "#EA0004"
      - "#F29A37"
      - "#FFD100"
      - "#FFEC00"
      - "#00CE00"
      - "#009900"
      - "#0461C6"
      - "#063d79"
    dark:
      - "#000000"
      - "#6A6A6A"
      - "#919191"
      - "#AEAEAE"
      - "#C5C5C5"
      - "#DADADA"
      - "#EDEDED"
      - "#ffffff"
      - "#EA0004"
      - "#F29A37"
      - "#FFD100"
      - "#FFEC00"
      - "#00CE00"
      - "#009900"
      - "#0461C6"
      - "#063d79"

css-variables:
  font-family: Roboto
  font-size-base: 28px
  inactive-color: lightgrey
  icon-size: 2vmin
  margin-columns: 1em
  block-border-width: 1rem
  block-border-padding: 1rem
  block-border-style: solid
  vertical-margin: 0.7rem

template:
  title-page:
    vertical-margin: 1em
    teaser-img:
      height: 200px
      margin: 50px
    affiliation-logo:
      height: 70px

math:
  scale: 0.9
  macros:
    R: '{{\mathrm{{I}\kern-.15em{R}}}}'
    laplace: '{\Delta}'
    grad: '{\nabla}'
```

## JMU Title Slide Keys

These keys are consumed directly by `template/deck-title.html`.

| Key | Shape | Usage |
| --- | --- | --- |
| `template.title-page.banner` | path | Optional banner image at the top of the title slide. |
| `template.title-page.teaser-img.file` | path | Optional title-slide teaser image. |
| `template.title-page.teaser-img.height` | CSS/image length | Height attribute for the teaser image. |
| `template.title-page.teaser-img.margin` | CSS length | Inline top and bottom margin for the teaser image wrapper. |
| `template.title-page.affiliation-logo.file` | path | Optional title-slide affiliation logo. |
| `template.title-page.affiliation-logo.height` | CSS/image length | Height attribute for the affiliation logo. |
| `title`, `subtitle`, `date`, `author`, `affiliation` | scalar | Visible title-slide text fields. |

The pack README also shows a legacy shorthand where `template.title-page.teaser-img` and `template.title-page.affiliation-logo` are scalar paths. The current template expects the nested `.file` form for those two image fields.

Example skeleton:

```yaml
title: "Presenting with Decker"
subtitle: "A Guide to Creating Memorable Presentations"
date: "2026-09-22"
author: "Samantha Monty"
affiliation: "Chair for Human-Computer Interaction, University of Wuerzburg"

template:
  title-page:
    banner: assets/jmu-hci-banner.jpg
    teaser-img:
      file: assets/title-teaser.jpg
      height: 200px
      margin: 50px
    affiliation-logo:
      file: assets/hci-jmu-logo.png
      height: 70px
```

## JMU Index Page Schema

The generated index template uses `index_page` as its custom schema. When `index_page` is absent, the template falls back to the standard generated deck/page/quest lists.

| Key | Shape | Usage |
| --- | --- | --- |
| `index_page.chatty.server` | URL | Server written into the `chatty-data` JSON script for the index Chatty UI. |
| `index_page.chatty.prompt` | string | Prompt id written into index Chatty data. |
| `index_page.chatty.greeting` | string | Optional greeting read by `support/js/index.js`; falls back to global `chatty.greeting` and then localization. |
| `index_page.chatty.write-markdown` | boolean | Shown in README example; not directly consumed by current template search, likely intended for Chatty generation workflow. |
| `index_page.defaults.chapters.materials.teaser-img` | path | Default teaser image stored as `data-default-teaser` on `<body>` and consumed by `index-styling.js`. |
| `index_page.defaults.chapters.materials.teaser-id` | integer/string | Default teaser id stored as `data-default-teaser-id`; JS resolves it against images found in target pages. |
| `index_page.menu_bar.items` | list | Top-level custom menu items. |
| `index_page.menu_bar.items.label` | string | Menu label. |
| `index_page.menu_bar.items.action` | URL/path | Optional direct link; if absent, the item becomes a submenu toggle. |
| `index_page.menu_bar.items.submenu` | list | Second-level menu items. |
| `index_page.menu_bar.items.submenu.label` | string | Second-level label. |
| `index_page.menu_bar.items.submenu.action` | URL/path | Second-level direct link. |
| `index_page.menu_bar.items.submenu.submenu` | list | Third-level menu items. |
| `index_page.menu_bar.items.submenu.submenu.label` | string | Third-level label. |
| `index_page.menu_bar.items.submenu.submenu.action` | URL/path | Third-level direct link. |
| `index_page.image_carousel.items` | list | Images rendered into the banner carousel. |
| `index_page.image_carousel.items.image` | path | Carousel image source. |
| `index_page.image_carousel.items.caption` | string | Present in README example; not currently emitted by the template. |
| `index_page.image_carousel.items.duration` | number | Present in README example; not currently emitted by the template. |
| `index_page.about.course-description` | string/blocks | About section course description. |
| `index_page.about.pre-requisites` | string/blocks | About section prerequisites. |
| `index_page.about.target-audience` | string/blocks | About section target audience. |
| `index_page.about.learning-objectives` | list | Learning objective entries. |
| `index_page.about.learning-objectives.objective` | string | Objective text. |
| `index_page.about.contacts` | list | Course contact entries. |
| `index_page.about.contacts.name` | string | Contact name. |
| `index_page.about.contacts.email` | string | Contact email. |
| `index_page.about.contacts.reason` | string | Reason shown as "Contact for ...". |
| `index_page.chapters` | list | Course chapter groups. |
| `index_page.chapters.topic` | string | Chapter heading. |
| `index_page.chapters.collapsed` | boolean | Initial collapsed state for generated card groups. |
| `index_page.chapters.materials` | list | Material/card entries in a chapter. |
| `index_page.chapters.materials.hidden` | boolean | Suppresses a material from output when true. |
| `index_page.chapters.materials.title` | string | Card/material title. |
| `index_page.chapters.materials.date` | string | Card date; if absent, JS tries to fetch it from the linked slide HTML metadata. |
| `index_page.chapters.materials.description` | string | Card description; if absent, JS tries linked slide metadata. |
| `index_page.chapters.materials.slides` | path/URL | Main link target for the material card. |
| `index_page.chapters.materials.teaser-img` | path | Per-material teaser image. |
| `index_page.chapters.materials.teaser-id` | integer/string | Per-material teaser selector resolved by JS against linked page images. |
| `index_page.chapters.materials.keywords` | list/string | Keywords rendered into hidden/source markup and used in cards/filtering. Prefer YAML lists even though the README example shows a comma-separated scalar. |
| `index_page.chapters.materials.files` | list | Additional file/action links for a material. |
| `index_page.chapters.materials.files.name` | string | Link tooltip/text. |
| `index_page.chapters.materials.files.link` | path/URL | Link target. |
| `index_page.chapters.materials.files.icon` | CSS class string | Icon class, for example Font Awesome classes. |
| `index_page.chapters.materials.files.color` | CSS color | Inline icon color. |

Example skeleton:

```yaml
index_page:
  chatty:
    server: https://example.edu
    prompt: pmpt_...
    greeting: "Ask about this course."
    write-markdown: true
  defaults:
    chapters:
      materials:
        teaser-img: assets/default-card.jpg
        teaser-id: 0
  menu_bar:
    items:
      - label: External Resources
        submenu:
          - label: Course Website
            action: https://example.edu/course
          - label: Nested Group
            submenu:
              - label: Nested Link
                action: https://example.edu/nested
  image_carousel:
    items:
      - image: assets/teaser1.jpg
        caption: "Welcome"
        duration: 5
  about:
    course-description: Short course description.
    pre-requisites: None
    target-audience: Students interested in HCI.
    learning-objectives:
      - objective: Understand the core topics.
    contacts:
      - name: Course Team
        email: course@example.edu
        reason: organization
  chapters:
    - topic: Introduction
      collapsed: false
      materials:
        - title: Welcome
          hidden: false
          date: "2026-09-22"
          description: First lecture.
          slides: welcome-deck.html
          teaser-img: assets/welcome.jpg
          teaser-id: 1
          keywords:
            - introduction
            - hci
          files:
            - name: Handout
              link: welcome-handout.html
              icon: fa-solid fa-file-lines
              color: "#0049b7"
```

## Other Template Variables Used By JMU

| Key | Usage |
| --- | --- |
| `template.base-css` | Optional base stylesheet in page templates. |
| `template.css`, `template.deck.css` | CSS injection points. |
| `template.js`, `template.js-module`, `template.deck.js`, `template.deck.js-module` | Script injection points. |
| `template.index-page.banner` | Optional banner in page/index header templates. |
| `template.favicon` | Favicon in generated pages/decks. |
| `toc`, `table-of-contents`, `toc-title`, `idprefix` | Page/index table of contents and id-prefix controls. |
| `highlightjs`, `highlighting-css` | Syntax highlighting controls. |
| `poll`, `live-captions`, `reveal-plugins`, `codapi.server` | Conditional plugin/runtime inclusion. |
| `decks.by-title`, `pages.by-title`, `quests.by-title` | Standard generated index fallback collections. |
| `title`, `subtitle`, `author`, `authors`, `affiliation`, `copyright`, `date`, `keywords`, `description-meta` | Standard metadata rendered by JMU templates. |

Example skeleton:

```yaml
template:
  favicon: assets/favicon.ico
  base-css: assets/base.css
  css:
    - assets/jmu-overrides.css
  js-module:
    - assets/jmu-global.js
  deck:
    css:
      - assets/jmu-deck.css
    js-module:
      - assets/jmu-deck.js
  index-page:
    banner: assets/index-banner.jpg

toc: true
table-of-contents: true
toc-title: Contents
idprefix: jmu-

highlightjs: atom-one-light
poll: false
live-captions: true

reveal-plugins:
  - name: CustomPlugin
    url: assets/custom-plugin.js

codapi:
  server: https://codapi.example.edu

description-meta: "Short course index description."
keywords:
  - hci
  - lecture
```

## Example Skeleton

```yaml
resource-pack: jmu-hci-drp
static-resource-dirs:
  - assets

template:
  css:
    - assets/course.css
  title-page:
    banner: assets/jmu-hci-banner.jpg
    teaser-img:
      file: assets/title-teaser.jpg
      height: 200px
      margin: 50px
    affiliation-logo:
      file: assets/hci-jmu-logo.png
      height: 70px

index_page:
  chatty:
    server: https://example.edu
    prompt: pmpt_...
  defaults:
    chapters:
      materials:
        teaser-img: assets/default-card.jpg
  menu_bar:
    items:
      - label: External Resources
        submenu:
          - label: Course Website
            action: https://example.edu/course
  image_carousel:
    items:
      - image: assets/teaser1.jpg
  about:
    course-description: Short course description.
    pre-requisites: None
    target-audience: Students interested in HCI.
    learning-objectives:
      - objective: Understand the core topics.
    contacts:
      - name: Course Team
        email: course@example.edu
        reason: organization
  chapters:
    - topic: Introduction
      collapsed: false
      materials:
        - title: Welcome
          date: 2026-09-22
          description: First lecture.
          slides: welcome-deck.html
          teaser-img: assets/welcome.jpg
          files:
            - name: Handout
              link: welcome-handout.html
              icon: fa-solid fa-file-lines
              color: "#0049b7"
```
