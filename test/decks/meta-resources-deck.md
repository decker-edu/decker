---
csl: 'acm-sig-proceedings.csl'
deep:
  down:
    bool: true
static-resource-dirs:
- code
meta-data:
- 'chart-meta.yaml'
- level1.yaml
something: '/test/decks/include/06-metal.png'
template:
  css: dummy.css
title: Meta Resources
---

# Meta Resources

Resource references in meta data values

-   `css:` [:meta](css)
-   `template.css:` [:meta](template.css)
-   `csl:` [:meta](csl)
-   `deep.down.bool:` [:meta](deep.down.bool)
-   Slide background should be gray

# Template default meta data

## From `resource/template/default.yaml`

-   `history:` [:meta](history)
-   `fragments:` [:meta](fragments)
-   `controls:` [:meta](fragments)

## Meta Data from `meta-data`

-   `chartId`: [:meta](chartId)
-   `data-chart`: [:meta](data-chart)

# File lists

## Static data directories

-   `static-resource-dirs`: [:meta](static-resource-dirs)

# File lists

## Excluded directories

-   `exclude-directories`: [:meta](exclude-directories)

# Transitive meta data files

## Inclusion chain

``` {.txt}
meta-resources-deck.md <- level1.yaml <- include/level2.yaml <- 06-metal-v.png
```

## Absolute image path

-   metal-image: [:meta](metal-image)
