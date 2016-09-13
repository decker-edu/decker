---
author: Henrik Tramberend
date: '15.5.2016'
subtitle: Tutorial and Examples
theme: 'htr-slides'
title: Decker Slide Tool
transition: linear
---

# Overview

-   Features
-   Installation
-   Usage
-   Development

# Features {.section}

# Markdown Syntax

## Pandoc-Markdown

-   Slides are basically [Pandoc-Markdown](http://pandoc.org) formatted
    text
-   Pandoc provides a Markdown variant with many extensions

## Some Pandoc extensions

-   Bibliographies
-   Footnotes
-   Tables
-   Figures with captions
-   Code blocks with syntax highlighting
-   LaTeX math typesetting

# Slides

## Markdown header

-   Level 1 header (`#`) starts new slide
-   Level 2 header (`##`) starts a block on a slide
-   Level 3 header (`###`) starts new column on a slide

``` {.markdown}
# Episode IV: A new Slide

## A long time ago ...

... in a galaxy far, far away.
```

# Multicolumn slides

## The author

![](img/htr-beuth.jpg)

###

## Slide source

``` {.markdown}
# Multicolumn slides

## The author

![](img/htr-beuth.jpg)

###

## Slide source

~~~ {.markdown}
∞
~~~
```

# Local Images

## Relative path

![](img/06-metal.png){width=75%}

# LaTeX Math

## Syntax

-   Standard LaTeX syntax
-   Single \$ encloses inline math
-   Double \$\$ encloses a display math block

## Example

-   To $\infty$ and beyond!

$$
e = mc^2
$$

# Compile Time Macros

## Macros

-   Appropriated link `[]()` and image `![]()` syntax
-   Example: embed a YouTube video

    ``` {.markdown}
    ## Video

    ![:youtube](Wji-BZ0oCwg)
    ```

###

## Video

![:youtube](Wji-BZ0oCwg)

# Compile Time Templating

## Mustache templates

-   Markdown source code is processed with Mustache

    ``` {.markdown}
    {{=<% %>=}}
    The current semester is {{semester}}
    <%={{ }}=%>
    ```

-   Data is provided in YAML files

    ``` {.yaml}
    ---
    semester: Summer 2016
    ---
    ```

-   Results in

    ``` {.markdown}
    The current semester is Summer 2016
    ```

# Blocks

## Block markup

-   Level 2 headers start new block
-   Blocks can be marked with attributes

## Alert block {.alert}

-   This block is marked `alert`

``` {.markdown}
## Alert block {.alert}

- This block is marked `.alert`
```

###

## Block styles

-   Other block styles include `definition` and `equation`

## Definition {.definition}

$e=mc^2$

## Equation {.equation}

$e=mc^2$

# Speaker Notes

## Slide level

-   The slide content becomes part of the speaker notes
-   Add `notes` class to slide header

    ``` {.markdown}
    # Slide Level {.notes}

    These are speaker notes.
    ```

###

## Block level

-   Block content becomes part of the speaker notes
-   Add `notes` class to level two header

    ``` {.markdown}
    ## Block level {.notes}

    These are speaker notes too.
    ```

# These are just notes {.notes}

Slides with headers that are have the `.notes` class attribute are not
included in the presentation. They are only visible in the handout and
probably are available as presenter notes during slide presentation.

# Cached Images

## Local image cache

Remote images can be cached locally

Cache directory is named `img/cached` and is located in the directory of
the referencing document

`decker cache` scans for and downloads all images

###

## Cached remote image

![Some piece of scene
graph](http://mmi-rtr.dev/slides/02-jet-engine/img/cube-scene-graph.png)

# Meta Data

## Mustache template processor

-   Markdown source is processed by [mustache]()
-   Data is the union of all available YAML files

# Meta Data Example

## Markdown source

``` {.markdown}
Your total score is {\{total.score}}.
```

## YAML data

``` {.yaml}
---
total:
    score: 42
...
```

## Result

``` {.html}
Your total score is 42.
```

# `decker` Tool {.section}

# `decker` Tool

## Command line tool

-   Statically linked, no library dependencies
-   Easy to deploy, just copy the executable
-   Self-contained, no support or data files
-   Not configurable, conventions are key

## Implemented using

-   [Haskell](http://haskell.org)
-   [Pandoc](), [Shake](), [Mustache](), [reveal.js]()
-   [LaTeX](), [livereloadx](), [decktape.sh]()

# `decker` Targets

## Specialized build tool

-   `decker` is like `make` without the makefile
-   Operates on the current directory and below
-   Considers Markdown text files and YAML data files

## `decker` source files

-   `*-deck.md` is a slide deck
-   `*-page.md` is a one page document styled like an article
-   `*.md` is a general document without any semantic attached

# `decker` generated targets

## Generated from `*-deck.md`

-   `*-deck.html` a *reveal.js* based HTML slide deck
-   `*-deck.pdf` a PDF version of that deck
-   `*-handout.html` a HTML document containing only the speaker notes
    from the deck
-   `*-handout.pdf` a PDF version of that handout

## Generated from `*-page.md`

-   `*-page.hml` a HTML article page
-   `*-page.pdf` a PDF version of that article

# Slide Header

# Installation {.section}

# Usage {.section}

# Decker Commands

## `decker`

-   Recursively scans the current directory for Markdown files ending in
    `.md`

## `decker clean`

## `decker example`

## `decker server`
