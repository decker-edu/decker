# Project configuration (`decker.yaml`)

`decker.yaml` in the project root holds global meta data for every document.
A document's front matter overrides it. `decker info --json` shows the values
that matter to an agent, merged with the resource pack defaults.

```yaml {.decker-example file="decker.yaml"}
title: "Course Title"
author: "Author Name"
lang: en
exclude-directories:           # not scanned for sources
  - code
static-resource-dirs:          # copied verbatim to public/
  - static
experiments:
  fragment-templates: true     # see `decker agent-guide templates`
strict: true                   # fail builds that leave warnings
```

A single value can be set for one build with `-m key=value`, e.g.
`decker -m lang=de public/unit/07-deck.html`.

## How values combine

In order of increasing precedence: the defaults of the base pack, the defaults
of the resource pack, `decker.yaml`, its `meta-data` files, `-m`, the front matter,
and the `meta-data` files of the front matter.

- Maps merge key by key.
- Lists are joined, so front matter cannot remove an entry of `decker.yaml`.
- `index.links`, `palette.colors.light`, `palette.colors.dark` and all lists below
  `external-tools` replace the lower value.
- Paths starting with `/` are relative to the project root, others to the file
  that holds them.

## Keys

| Key                    | Meaning                                                      |
|------------------------|--------------------------------------------------------------|
| `resource-pack`        | Resource pack, e.g. `exe:mono` (bundled: `exe:mono`, `exe:mario`, `exe:tudo`, `exe:wburg`, `exe:nwburg`) |
| `meta-data`            | List of further YAML files merged into the meta data         |
| `exclude-directories`  | Paths from the project root, not a name to match anywhere: `archive` keeps `lectures/archive/` |
| `static-resource-dirs`, `static-resources` | Directories, or files and directories, copied to `public/` |
| `watch.exclude`        | Glob patterns that do not trigger a rebuild                  |
| `strict`               | Like `--strict` on every build                               |
| `publish.rsync.destination` | Target of `decker publish`                              |
| `bibliography`, `csl`  | Bibliography file and citation style (`decker agent-guide decks`) |
| `draft`                | Leaves the document out of the search index, `publish --lecture` and `decker chatty`. It is still built |
| `reveal.width`, `reveal.height` | Slide size, 1280 by 720 by default. All of `reveal` goes to Reveal.js |
| `template.css`, `template.js`, `template.js-module` | Lists of files to load in decks and pages. `template.css` is loaded on all generated documents, the scripts are not |
| `template.deck.css`, `template.deck.js`, `template.deck.js-module` | The same for decks only |
| `template.page.js`, `template.page.js-module` | Scripts for pages only     |
| `template.favicon`     | Icon of all generated documents                              |
| `css-variables`        | `font-size-base: 28px` declares `--font-size-base: 28px;`    |
| `math.macros`          | MathJax macros for decks                                     |
| `index.links`          | Links next to each deck on an index page: `a11y`, `handout`, `pdf`, `presenter` |
| `short-links`          | URL schemes: with `wiki: "https://en.wikipedia.org/wiki/@@"`, `[a](wiki:Foo)` is a link |
| `external-tools`       | Programs Decker runs, `decker check` shows which are found   |

## Plugins and features

`decker-plugins` switches `zoom`, `jingles`, `quiz`, `quiz-wue`, `feedback`, `notes`,
`chatty` and `quizzer`. `page-features` switches `codapi` and `math-module` for pages
and index pages. Packs choose their own defaults. `mono` turns `quiz` off, for
example, so its decks do not load the quiz plugin. Check `config.deckerPlugins` and
`config.pageFeatures` in `decker info --json` before you use such a feature.

## Title slide

The keys are read from the front matter or from `decker.yaml`. Paths are found
and copied to `public/` automatically:

```markdown {.decker-example file="intro-deck.md" project="title"}
---
title: Introduction
subtitle: The first lecture
date: today
authors:
  - name: Ada Lovelace
    affiliation:
      name: Example University
      logo:
        uri: /logo.svg
        height: 2rem
teaser: /logo.svg
logos:
  - /logo.svg
---

# First slide
```

```xml {.decker-example file="logo.svg" project="title"}
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 1 1"><rect width="1" height="1"/></svg>
```

## Typical layout

```
project/
├── decker.yaml        # configuration
├── index.md           # optional; otherwise an index is generated
├── *-deck.md, *-page.md, *-quest.yaml   (anywhere below the root)
├── templates/         # project fragment templates (optional)
├── public/            # build output (generated, not committed)
└── private/           # private build output, e.g. question previews
```

See the [users' guide](https://github.com/decker-edu/decker/blob/develop/users-guide/decker-users-guide.md), sections *Meta Data*, *Configuration reference* and *Resources*.
