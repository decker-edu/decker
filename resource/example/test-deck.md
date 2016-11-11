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

# Includes

## Include markdown files

The following text ist included from file `/resource/realtive.md`:

[#include](/resource/relative.md)

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

-   Appropriated link `[Link text](Url)` and image `![Alt text](Url)` syntax
-   Example: embed a YouTube video

    ``` {.markdown}
    ## Video

    [:youtube](Wji-BZ0oCwg)
    ```

###

## Video

[:youtube](Wji-BZ0oCwg)

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
