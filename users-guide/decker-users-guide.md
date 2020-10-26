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

Decker uses the universal markup converter
[Pandoc](https://pandoc.org/MANUAL.html#pandocs-markdown) to translate
slide content in Markdown format to interactive HTML slide decks. A
working knowledge of the Pandoc dialect of Markdown is very helpful when
working with Decker.

-   [Pandoc User's
    Guide](https://pandoc.org/MANUAL.html#pandocs-markdown)

This document mainly describes additional features and conventions that
Decker adds to Pandoc's Markdown.

## Reveal.js

## Features

## Using Decker

## Creating a project

## Working on a project

## Publishing

Decker can use a locally installed [Rsync](https://rsync.samba.org) to
publish the entire project to a remote location with the command

``` {.sh}
> decker publish
```

The remote location is specified in the meta data variable
`publish.rsync.destination:` using the URL formats that Rsync
understands. For example, to publish the entire project directly into
the document directory of a remote webserver the `decker.yaml` file
would contain:

``` {.yaml}
publish:
  rsync:
    destination: author@public.server.com:/var/www/html/cg-lectures
```

To more precisely control the behaviour of Rsync, a list of options can
be specified in the variable `publish.rsync.options`. For example, to
*mirror* (as opposed to *copy* ) the public directory to the destination
the setting would be:

``` {.yaml}
publish:
  rsync:
    destination: author@public.server.com:/var/www/html/cg-lectures
    options: 
      - --delete
```

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

`slides/slide-deck.md` contains:

``` {.markdown}
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
:   allow vertical slides (defaults to `true`)

### Dictionary

Decker has some content that can be adapted to the language of the
presentation. This is a work-in-progress and is currently used for
quizzes.

The current default dictionary looks like this:

    dictionary:
      de: 
        quiz:
          solution-button: Lösung
          input-placeholder: Eingeben und 'Enter'
          qmi-drag-hint: Objekte per Drag&Drop ziehen...
          qmi-drop-hint: ...und hier in die richtige Kategorie einsortieren.
      en:
        quiz:
          solution-button: Show Solution
          input-placeholder: Type and press 'Enter'
          qmi-drag-hint: Drag items from here...
          qmi-drop-hint: ...and put them here into the correct category.

This dictionary can be partially or completely defined new by the user.

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

## Questions

The audience of a deck can annotate slides with questions. The questions
are aggregated on a server and are visible by all audience members and
the author.

The slide author can later choose to address the questions by changeing
or extending the information in the deck.

To enable this feature a deck must specify the URL of a Decker Engine
server in the meta data by setting the variable
`decker-engine-base-url`. For example:

``` {.yaml}
decker-engine-base-url: 'https://tramberend.beuth-hochschule.de/decker'
```

### Endpoints with authorization

There are two modes of operation depending on the deployment details of
the server, *authorized* and *public*.

If the server is running behind a proxy with Basic Authentication
enabled, questions can only be added if the user has been authenticated
by the proxy. Administrators are recognized automatically, no further
authorization is necessary.

The `de-api` endpoint works that way:

``` {.yaml}
decker-engine-base-url: 'https://tramberend.beuth-hochschule.de/de-api'
```

### Public endpoints

If the server is publicly available without authentication, the user is
assigned a token that allows her to later delete or edit all questions
that where added using that token. The token can be entered by hand or
stored in the browser's local storage. Administrators need to
authenticate with a username and a password.

The `decker` endpoint works that way:

``` {.yaml}
decker-engine-base-url: 'https://tramberend.beuth-hochschule.de/decker'
```

### Admistrators

Users that are authorized as administrators can edit and delete all
questions in a set.

## Quizzes

### Class definition

For each question type you can use either of the three tags to create
quizzes

    .quiz-match-items, .quiz-mi, .qmi

    .quiz-multiple-choice, .quiz-mc, .qmc

    .quiz-insert-choices, .quiz-ic, .qic 

    .quiz-free-text, .quiz-ft, .qft

### Basic syntax

The quiz syntax is based on the markdown task list syntax. A markdown
task list looks like this

    - [ ] This box is not checked
    - [X] This box is checked
    - [ ] Another unchecked box

Questions are defined by level 2 headers. That means creating a question
**needs**

    ## Question title {.qmc}

(where `.qmc` can be replaced by any of the other quiz classes)

You can add tooltips by creating a nested list e.g.

    - [ ] A
      - tooltip A
    - [X] B
      - tooltip B

### Fenced Divs Syntax

Alternatively, quizzes can be defined using the **fenced divs** syntax:

    ::: qmc
    - [ ] A
      - tooltip A
    - [X] B
      - tooltip B
    :::

### Matching Questions

These questions generate quizzes where a user can drag and drop items to
sort them into "buckets".

This uses the Pandoc [definition list
syntax](https://pandoc.org/MANUAL.html#definition-lists).

You can provide distractor items (items not belonging to any bucket) or
empty buckets (no item belonging in those empty buckets) by using the
exclamation mark "!".

    ## Matching Question {.qmi}

    Question text

    BucketA
    : A1
    : A2

    BucketB
    : B1

    !
    : Distractor

    Empty Bucket
    : !

### Multiple Choice Questions

Classic multiple choice questions

    ## Multiple Choice Question {.qmc}

    Question text

    - [ ] A
      - nope
    - [X] B
      - yes

### InsertChoices Questions

This will create a sort of blank text questions. If multiple items are
provided in the task list, they will be rendered as a drop down menu
where the user can click answers.

If only one item/solution is provided it will be rendered as a blank.

    ## Insert Choices Question {.qic}

    - [X] A
      - of course
    - [ ] B 
      - uhm ...

    is the first letter in the ABC. The second one is

    - [ ] B
      - yep

### FreeText questions

This will create a simple input field/text box where the user can write
their answer.

    ## FreeText Question TL {.qft}

    What's the first letter in the alphabet?

    - A
      - yep
    - B
      - nope

    ## {.qft}

    What's the fourth letter?

    - [ ] C
    - [X] D

### Quiz Meta

Add a `YAML` code block to a question to provide meta information on the
specific question.

This is work in progress. Currently apart from `lang: de` or `lang: en`
it does not do anything. (21. Jul 2020)

    ``` {.yaml}
    lang: de
    score: 5
    category: FP
    lectureId: fp1
    topic: Functional Programming Introduction
    ```

## ThebeLab 🚧 {#thebelab}

## Sage

## GraphViz

## Gnuplot

# Hacking on Decker

## Conventions

### Commit emoji convention

(Lifted from https://spacevim.org/conventions/.)

-   :memo: Add comment or doc.
-   :gift: New feature.
-   :bug: Bug fix.
-   :bomb: Breaking compatibility.
-   :white_check_mark: Write test.
-   :fire: Remove something.
-   :beer: I'm happy like reduced code complexity.
