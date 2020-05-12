---
css: './decker-users-guide.css'
lang: 'en-US'
title: 'Decker User''s Guide'
toc-title: Contents
---

🚧 Material that is only relevant for advanced users, developers, true
Decker nerds and Mario is marked with a construction sign (🚧). Eltern
haften für ihre Kinder.

# Description

## Pandoc

## Reveal.js

## Features

## Using Decker

## Creating a project

## Working on a project

## Publishing

# Options

# Meta Data

Meta data variables are specified in YAML format and can be defined in
four different places. In order of increasing precedence these are:

-   the optional `decker.yaml` file that is read from the project's root
    directory
-   the `-m key=value` option on the `decker` command line (NOT YET
    IMPLEMENTED)
-   additional meta data files specified in the meta data variable
    `meta-data`
-   the meta data sections of the slide source Markdown file

Meta data is hierarchical but most variables are defined at the top
level. A notable exception are variables that are used to set *local
path* values (see [Local paths](#local-paths)) in the slide template
(see [Variables for Reveal.js](#variables-revealjs)). These path values
are located in the `template` namespace. For example, the value for the
optional title teaser image is provided in the variable
`template.title-teaser`.

Inside a YAML file or a YAML section of a file hierarchical meta
variable values are defined as follows:

``` {.yaml}
template:
    title-teaser: /images/teaser.png
```

On the command line this can be specified as

``` {.sh}
decker -m 'template.title-teaser=/images/teaser.pn'
```

## Local paths {#local-paths}

Paths to local file resources that are referenced by slide sets need to
be provided in several contexts. For example

-   as a URL in an image tag to locate local media files like images or
    videos

    ``` {.markdown}
    ## A very important image
    ![](image.png)
    ```

-   as the value of meta data variable, for example to provide the
    location of the bibliography database and the citation style
    definition

    ``` {.yaml}
    bibliography: /bib/bibliography.tex
    csl: /bib/chicago-author-data.csl
    ```

In any case, path values for local file resources are interpreted either
as relative to the defining file, if specified as a *relative path*, or
as relative to the project root directory, if specified as an *absolute
path*.

Consider the following project layout and file contents

``` {.txt}
project
├── images
│   └── image.png
└── slides
    └── slide-deck.md
```

`slides/slide-deck.md`:

:   ``` {.markdown}
    # First slide
    ![Project relative path](/images/image.png)
    ![Document relative path](../images/image.png)
    ```

Both image paths reference the same image file.

## Variables for Reveal.js {#variables-revealjs}

Decker uses a modified version of the standard pandoc template for
reveal.js slide sets. Most if the variables used there are supported by
decker. Additionally, there are several Decker specific variables that
control various aspects of the generated slide sets.

`align-global`
:   default alignment for various slide elements (defaults to `left`)

`mario` 🚧
:   use Mario's CSS for the slides (defaults to `false`)

`template.base-css` 🚧
:   the first CSS file that is loaded by the template (defaults to `''`)

`template.css` 🚧
:   alist of CSS files that is loaded after the default CSS files
    (defaults to `[]`)

`template.title-header`
:   a header image for the title slide (defaults to `''`)

`template.title-teaser`
:   an image that is placed below the title line on the title slide
    (defaults to `''`)

`template.affiliation-logo`
:   an imge that is placed above the affiliation information on the
    title slide (defaults to `''`)

`template.include-js` 🚧
:   a list of Javascript files that are included into the slide deck
    before Reveal.js is initialized (defaults to `[]`)

`style` 🚧

:   a list of CSS styles that are inserted into the HTML header
    (defaults to `[]`)

    For example, to set the background color of a all H2 header elements
    to red specify:

    ``` {.yaml}
    style:
      - 'h2 { backgroundColor: #f00; }' 
    ```

`thebelab.enable` 🚧
:   enable ThebeLab for the deck (defaults to `false`, see
    [ThebeLab](#thebelab))

`checkOverflow`
:   mark overrflowing slides with a red border (defaults to `false`)

`vertical-slides`
:   allow vertical slides (defaults to `false`)

# Decker's Markdown

## Media handling

External media files like images or movies can be included in a
presentation in a variety of ways. The central mechanism is the standard
Markdown inline image tag as used by Pandoc.

``` {.markdown}
![Image caption](/path/to/image.ext){width="100%"}
```

Several parameters describing the image can be encoded:

`[Image caption]`
:   If the image caption inside the square brackets `[]` is provided,
    the image will be set with the caption text right below the image.
    The caption text may contain further Markdown markup.

`(/path/to/image.ext)`
:   The image itself is referenced with an URL inside the round brackets
    `()`. A relativ reference (as described in [RFC
    3986](https://tools.ietf.org/html/rfc3986#section-4.2)) here is
    interpreted as a path to a resource in the local file system that is
    either specified relative to the project's root directory or
    relative to the file containing the image tag.

`.ext`
:   The filename extension determines the media type of the image.
    Depending on the extension and media type the referenced resource
    may further be processed by decker to generate the final embedded
    media element.

`{width="100%"}`
:   The attributes annotation can be used to control various aspects of
    processing and presentation for the image, for example the width of
    the image relative to it's surrounding element (see [Local
    Paths](#local-paths)).

### Figures and captions

Embedded media will be rendered as a figure with caption if either

-   the square brackets of the image tag contain a caption text.

    ``` {.markdown}
    ![This is the caption text.](some/image.png)
    ```

-   Or the image tag occurs on an otherwise empty paragraph followed
    directly by another paragraph that starts with the word `Caption:`.
    The second paragraph provides the text for the caption

    ``` {.markdown}
    ![](some/image.png)

    Caption: This is the caption text.
    ```

### Images

### Pdfs

### 3D polygonal models

### Iframes

### Videos

### Video streams

### Audio

### Graphs and diagrams

## Slide layout

## Whiteboard

## Quizzes

## ThebeLab 🚧 {#thebelab}

## Sage

## GraphViz

## Gnuplot
