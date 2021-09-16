---
color:
  black: '\#000'
  magenta: '\#f0f'
  white: '\#fff'
csl: '/test/decks/acm-sig-proceedings.csl'
deep:
  down:
    bool: true
meta-data:
- 'chart-meta.yaml'
- level1.yaml
something: '/test/decks/include/06-metal.png'
static-resource-dirs*:
- assets
template:
  css: dummy.css
title: Meta Resources
whiteboard-background-color: '\#f0f'
---

# Meta Resources

Resource references in meta data values

-   `css:` [:meta](css)
-   `template.css:` [:meta](template.css)
-   `csl:` [:meta](csl)
-   `deep.down.bool:` [:meta](deep.down.bool)
-   Slide background should be gray

# Template default meta data

## From [:meta](template-source)

-   `history:` [:meta](history)
-   `hash:` [:meta](hash)
-   `fragments:` [:meta](fragments)
-   `controls:` [:meta](fragments)
-   `test:` [:meta](test)

## Meta Data from `meta-data`

-   `chartId`: [:meta](chartId)
-   `data-chart`: [:meta](data-chart)

# File lists

## Static data directories

-   `static-resource-dirs*`: [:meta](static-resource-dirs*)

# File lists

## Excluded directories

-   `exclude-directories*`: [:meta](exclude-directories*)

------------------------------------------------------------------------

# Transitive meta data files

## Inclusion chain

``` {.txt}
meta-resources-deck.md <- level1.yaml <- include/level2.yaml <- 06-metal-v.png
```

## Absolute image path

-   metal-image: [:meta](template.metal-image)

------------------------------------------------------------------------

# `template/default.yaml` (1) {.columns}

## Command {.left}

``` {.sh}
stack run -- decker
```

## Template source

``` {.yaml}
template-source: 
```

## `default.yaml`

``` {.yaml}
test: "default.yaml test value"
```

## Variable `test123` {.right}

-   expexted: `test123`
-   actual: [:meta](test123)

------------------------------------------------------------------------

# `template/default.yaml` (2) {.columns}

## Command {.left}

``` {.sh}
stack run -- decker
```

## Template source

``` {.yaml}
template-source: 'exe:'
```

## `default.yaml`

``` {.yaml}
test: "default.yaml test value"
```

## Values {.right}

-   expexted: `default.yaml test value`
-   actual: [](test)

------------------------------------------------------------------------

# Hex color values in meta data

## White

-   [:meta](color.white)

## Black

-   [:meta](color.black)

## Magenta

-   [:meta](color.magenta)
