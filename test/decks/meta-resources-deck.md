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

-   `css:` [:meta](css)
-   `template.css:` [:meta](template.css)
-   `csl:` [:meta](csl)
-   `deep.down.bool:` [:meta](deep.down.bool)
-   Slide background should be [:meta](palette.colors.light.2)

# Template default meta data

## From [:meta](template-source)

-   `reval.history:` [:meta](reveal.history)
-   `reval.hash:` [:meta](reveal.hash)
-   `reval.fragments:` [:meta](reveal.fragments)
-   `reval.controls:` [:meta](reveal.fragments)
-   `test:` [:meta](test)

## Meta Data from `meta-data`

-   `chartId`: [:meta](chartId)
-   `data-chart`: [:meta](data-chart)

# File lists

## Static data directories

-   `static-resource-dirs`: [:meta](static-resource-dirs)

# File lists

## Excluded directories

-   `exclude-directories`: [:meta](exclude-directories)

--------------------------------------------------------------------------------

# Transitive meta data files

## Inclusion chain

``` txt
meta-resources-deck.md <- level1.yaml <- include/level2.yaml <- 06-metal-v.png
```

## Absolute image path

-   metal-image: [:meta](template.metal-image)

--------------------------------------------------------------------------------

# `template/default.yaml` (1) {.columns}

## Command {.left}

``` sh
stack run -- decker
```

## Template source

``` yaml
template-source:
```

## `default.yaml`

``` yaml
test: "default.yaml test value"
```

## Variable `test123` {.right}

-   expexted: `test123`
-   actual: [:meta](test123)

--------------------------------------------------------------------------------

# `template/default.yaml` (2) {.columns}

## Command {.left}

``` sh
stack run -- decker
```

## Template source

``` yaml
template-source: 'exe:'
```

## `default.yaml`

``` yaml
test: "default.yaml test value"
```

## Values {.right}

-   expexted: `default.yaml test value`
-   actual: [](test)

--------------------------------------------------------------------------------

# Hex color values in meta data

## White

-   [:meta](color.white)

## Black

-   [:meta](color.black)

## Magenta

-   [:meta](color.magenta)

--------------------------------------------------------------------------------

# Meta data list merging

## One line each from

-   `default.yaml`
-   `decker.yaml`
-   This header

## Results in

-   `test.merge-list:` [:meta](test.merge-list)

--------------------------------------------------------------------------------

# Meta data overide chain

## Precedence

1.  Meta data block from this deck
2.  `decker.yaml`
3.  `default.yaml`

## Values

| variable                   | value                             | expected            |
|---------------------------|----------------------------------|--------------------|
| `test.only-default`        | [:meta](test.only-default)        | *From default.yaml* |
| `test.default-decker`      | [:meta](test.default-decker)      | *From decker.yaml*  |
| `test.default-decker-deck` | [:meta](test.default-decker-deck) | *From this deck*    |

--------------------------------------------------------------------------------

# What about theses?

## Centering: `reveal.center`

[:meta](reveal.center)

## Slide number: `reveal.slideNumber`

[:meta](reveal.slideNumber)
