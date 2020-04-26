---
csl: 'acm-sig-proceedings.csl'
css: dummy.css
deep:
  down:
    bool: true
meta-data:
- 'chart-meta.yaml'
- level1.yaml
something: '/test/decks/include/06-metal.png'
title: Meta Resources
---

# Meta Resources

Resource references in meta data values

-   `css:` [:meta](css)
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

# Transitive meta data file

## Inclusion chain

``` {.txt}
meta-resources-deck.md <- level1.yaml <- include/level2.yaml <- 06-metal-v.png
```

## Absolute image path

-   metal-image: [:meta](metal-image)
