---
color:
  black: "#000"
  magenta: "#f0f"
  white: "#fff"
csl: /test/decks/acm-sig-proceedings.csl
deep:
  down:
    bool: true
meta-data:
  - chart-meta.yaml
  - level1.yaml
rpv-test:
  branch:
    no-path: just some value
    path: /test/decks/data/just-used-in-template.png
  deeper:
    leaf-path: /test/decks/data/just-used-in-template.png
  leaf-no-path: just some value
  leaf-path: /test/decks/data/just-used-in-template.png
runtime-path-variables:
  - one.more.from.doc.meta
something: /test/decks/include/06-metal.png
static-resource-dirs:
  - assets
template:
  css: dummy.css
test:
  default-decker-deck: From this deck
  merge-list:
    - This line is from this deck
  some-value: This line is from this deck
title: Meta Resources
whiteboard-background-color: "#f0f"
---

# Meta Resources

Resource references in meta data values

- `css:` [:meta](css)
- `template.css:` [:meta](template.css)
- `csl:` [:meta](csl)
- `deep.down.bool:` [:meta](deep.down.bool)
- Slide background should be [:meta](palette.colors.light.2)

# Template default meta data

## From [:meta](template-source)

- `reval.history:` [:meta](reveal.history)
- `reval.hash:` [:meta](reveal.hash)
- `reval.fragments:` [:meta](reveal.fragments)
- `reval.controls:` [:meta](reveal.fragments)
- `test:` [:meta](test)

## Meta Data from `meta-data`

- `chartId`: [:meta](chartId)
- `data-chart`: [:meta](data-chart)

# File lists

## Static data directories

- `static-resource-dirs`: [:meta](static-resource-dirs)

# File lists

## Excluded directories

- `exclude-directories`: [:meta](exclude-directories)

---

# Transitive meta data files

## Inclusion chain

```txt
meta-resources-deck.md <- level1.yaml <- include/level2.yaml <- 06-metal-v.png
```

## Absolute image path

- metal-image: [:meta](template.metal-image)

---

# `template/default.yaml` (1) {.columns}

## Command {.left}

```sh
stack run -- decker
```

## Template source

```yaml
template-source:
```

## `default.yaml`

```yaml
test: "default.yaml test value"
```

## Variable `test123` {.right}

- expexted: `test123`
- actual: [:meta](test123)

---

# `template/default.yaml` (2) {.columns}

## Command {.left}

```sh
stack run -- decker
```

## Template source

```yaml
template-source: "exe:"
```

## `default.yaml`

```yaml
test: "default.yaml test value"
```

## Values {.right}

- expexted: `default.yaml test value`
- actual: [](test)

---

# Hex color values in meta data

## White

- [:meta](color.white)

## Black

- [:meta](color.black)

## Magenta

- [:meta](color.magenta)

---

# Meta data list merging

## One line each from

- `default.yaml`
- `decker.yaml`
- This header

## Results in

- `test.merge-list:` [:meta](test.merge-list)

---

# Meta data overide chain

## Precedence

1.  Meta data block from this deck
2.  `decker.yaml`
3.  `default.yaml`

## Values

| variable                   | value                             | expected            |
| -------------------------- | --------------------------------- | ------------------- |
| `test.only-default`        | [:meta](test.only-default)        | _From default.yaml_ |
| `test.default-decker`      | [:meta](test.default-decker)      | _From decker.yaml_  |
| `test.default-decker-deck` | [:meta](test.default-decker-deck) | _From this deck_    |

---

# What about these?

## Centering: `reveal.center`

[:meta](reveal.center)

## Slide number: `reveal.slideNumber`

[:meta](reveal.slideNumber)

# Meta data files in `decker.yaml`

## In `default.yaml`

```yaml
meta-data:
  - test/decks/data/one-meta-value.yaml
```

## In `test/decks/data/one-meta-value.yaml`

```yaml
included-meta-value: "This was included from test/decks/data/one-meta-value.yaml"
```

## Observed value of `included-meta-value`

[:meta](included-meta-value)

---

# Runtime Path Variables

Metadata variables that contain project absolute or document relative file
system paths

## `runtime-path-variables`

A list of paths into the metadata variable name hierarchy

Current value
: [:meta](runtime-path-variables)

# Runtime Path Variables (2)

```yaml
rpv-test:
  leaf-path: /test/decks/data/07-moveable-camera-480x270-1000-zero.png
  leaf-no-path: "just some value"
  branch:
    path: /test/decks/data/07-moveable-camera-480x270-1000-zero.png
    deeper:
      path: /test/decks/data/07-moveable-camera-480x270-1000-zero.png
    no-path: "just some value"
```

- `rpv-test.leaf-path`: [:meta](rpv-test.leaf-path)
- `rpv-test.leaf-no-path`: [:meta](rpv-test.leaf-no-path)
- `rpv-test.branch.path`: [:meta](rpv-test.branch.path)
- `rpv-test.branch.deeper.path`: [:meta](rpv-test.branch.path)
- `rpv-test.branch.no-path`: [:meta](rpv-test.branch.no-path)
