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

Other common keys:

| Key                    | Meaning                                                      |
|------------------------|--------------------------------------------------------------|
| `resource-pack`        | Resource pack, e.g. `exe:mono` (bundled: `exe:mono`, `exe:mario`, `exe:tudo`, `exe:wburg`, `exe:nwburg`) |
| `bibliography`, `csl`  | Bibliography file and citation style (`decker agent-guide decks`) |
| `strict`               | Like `--strict` on every build                               |
| `meta-data`            | List of further YAML files merged into the meta data         |

A single value can be set for one build with `-m key=value`, e.g.
`decker -m lang=de public/unit/07-deck.html`.

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

See the [users' guide](https://github.com/decker-edu/decker/blob/develop/users-guide/decker-users-guide.md), sections *Meta Data* and *Resources*.
