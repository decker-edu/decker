---
css: ./decker-users-guide.css
lang: en-US
title: Decker User's Guide
toc-title: Contents
---

🚧 Material that is only relevant for advanced users, developers, true Decker
nerds and Mario is marked with a construction sign (🚧). Eltern haften für ihre
Kinder.

# Description

## Pandoc

Decker uses the universal markup converter
[Pandoc](https://pandoc.org/MANUAL.html#pandocs-markdown) to translate slide
content in Markdown format into interactive HTML slide decks. A working
knowledge of the Pandoc dialect of Markdown is very helpful when working with
Decker.

-   [Pandoc User's Guide](https://pandoc.org/MANUAL.html#pandocs-markdown)

This document mainly describes additional features and conventions that Decker
adds to Pandoc's Markdown.

## Reveal.js

## Features

## Using Decker

## Creating a project

## Working on a project

## Publishing

Decker can use a locally installed [Rsync](https://rsync.samba.org) to publish
the entire project to a remote location with the command

``` sh
> decker publish
```

The remote location is specified in the meta data variable
`publish.rsync.destination:` using the URL formats that Rsync understands. For
example, to publish the entire project directly into the document directory of a
remote webserver the `decker.yaml` file would contain:

``` yaml
publish:
  rsync:
    destination: author@public.server.com:/var/www/html/cg-lectures
```

To more precisely control the behaviour of Rsync, a list of options can be
specified in the variable `publish.rsync.options`. For example, to *mirror* (as
opposed to *copy* ) the public directory to the destination the setting would
be:

``` yaml
publish:
  rsync:
    destination: author@public.server.com:/var/www/html/cg-lectures
    options: 
      - --delete
```

# Targets

Decker uses [Shake](https://shakebuild.com) as it's underlying build and
dependency tracking system.

## `> decker version`

## `> decker decks`

## `> decker html`

## `> decker observed`

## `> decker info`

## `> decker check`

## `> decker publish`

## `> decker search-index`

Builds an inverted index over all Markdown source files and stores it in JSON in
`public/index.json`. The index can be used to implement incremental live-search
over all slides inside a Decker project.

This may well be a little time consuming, so it ist best called only right
before `decker publish`.

Decks that have set `draft: true` or whose (feedback) deck ids are in the global
`no-index:` list are removed from the search index.

# Commands

Commands do not engage the global dependency checking and will not trigger
rebuilds.

## `> decker clean`

Removes the `public` directory.

## `> decker purge`

Removes the `public` directory and the `.decker` directory where many things are
cached during compilation. Expect increased compilation times after a purge.

## `> decker example`

## `> decker pdf`

Compiles PDF documents for all HTML decks. It starts a headless Chrome browser
and uses it's printing capabilities to do that.

This may well be a little time consuming, so it ist best called only right
before `decker publish`.

## `> decker crunch`

Uses `ffmpeg` to concatenate and transcode raw video recordings of presentation
sessions. All WebM video segments for a presentation are combined and trancoded
into a single MP4 container that uses considerably less space on disk and in
transit.

## `> decker transcribe`

Uses `whisper.cpp` to transcribe video recordings to the recorded language and
English. Transcriptions are stored alongside the video files in `VTT` format and
are automatically available during playback.

[`whisper.cpp`]() needs to be installed locally and configured accordingly in
`decker.yaml`. Configuration meta data variables are:

``` yaml
# whisper.cpp transcription settings
whisper:
  base-dir: /usr/local/share/whisper.cpp
  model: models/ggml-large.bin
  lang: de
```

# Options

## `-h`, `--help`

List all decker commands, targets and options.

## `-m key=value`, `--meta="key=value"`

Specifies meta data variables (see below).

## `-S`, `--server`

Serve the public dir via HTTP (implies --watch).

## `-w`, `--watch`

Watch changes to source files and rebuild current target if necessary.

# Resources

Decker needs a lot of resources that are not contained in the decker source
file. Resource are all local data files that are required for proper operation
at *decker-run-time* or *deck-presentation-time*. Resources are highly specific
to the decker version that uses them.

In particular these are:

1.  Pandoc template files in `resource/template` (decker-run-time)
2.  HTML support files in `resource/support` (deck-presentation-time)
3.  Example presentation source files in `resource/example` (decker-run-time)

## Resource Packs

-   Template, support and example resources are combined into a *resource pack*

-   A resource pack may contain a `default.yaml` file with meta information
    regarding pack author etc. and default values for decker operation

-   The data from `default.yaml` is available in the meta data during slide
    compilation and presentation

-   A resource pack packed is the entire contents of the `resource` folder that
    must be made available during decker-run-time in one of three ways:

    1.  contained in the decker executable
    2.  contained in a local directory somewhere within a project
    3.  contained in a local ZIP archive somewhere within a project

-   Resource packs are located at run-time via their URL, three protocol schemes
    are supported:

    -   `exe:{name}` the `name` named resource pack that is located in the
        currently running executable (ie. `exe:tudo`)
    -   `{path}` the resource pack is located in a directory of the local file
        system within the project (ie. `resource-packs/tudo-official`)
    -   `{path}.zip` the resource pack is contained in a local ZIP archive
        within the project (ie. `resource-packs/tudo-informal`)

-   If localization, acquisition, unpacking or caching fails decker terminates

-   The local cache can be cleared with `decker purge`

-   The URL of the resource pack can be specified at runtime in the global
    `decker.yaml` metadata file, ie.

    ``` yaml
    resource-pack: `exe:tudo`
    ```

-   The default resource pack `exe:decker` is always loded first. Resources
    extracted from the resource pack in `resource-pack` simly overwrite and
    augment the default resources.

## Versioning

-   Resource bundles are always tied to a specific decker version
-   The exact decker version (MAJOR.MINOR.PATCH-LABEL) is always the last
    component of the resource file name
-   If decker is used with a non-matching resource bundle decker is terminated
    -   This can be down-graded to a warning with a meta data setting

# Meta Data

Meta data variables are specified in YAML format and can be defined in four
different places. In order of increasing precedence these are:

-   the `default.yaml` file that is read from the selected resource pack
-   the mandatory `decker.yaml` file that is read from the project's root
    directory
-   the `-m key=value` options on the `decker` command line
-   additional meta data files specified in the meta data variable `meta-data`
-   the meta data sections of the slide source Markdown file

Meta data is hierarchical but most variables are defined at the top level. A
notable exception are variables that are used to set *local path* values (see
[Local paths](#local-paths)) in the slide template (see [Variables for
Reveal.js](#variables-revealjs)). These path values are located in the
`template` namespace. For example, the value for the optional title teaser image
is provided in the variable `template.title-teaser`.

Inside a YAML file or a YAML section of a file hierarchical meta variable values
are defined as follows:

``` yaml
template:
    title-teaser: /images/teaser.png
```

On the command line this can be specified as

``` sh
decker -m 'template.title-teaser=/images/teaser.pn'
```

## Local paths {#local-paths}

Paths to local file resources that are referenced by slide sets need to be
provided in several contexts. For example

-   as a URL in an image tag to locate local media files like images or videos

    ``` markdown
    ## A very important image
    ![](image.png)
    ```

-   as the value of meta data variable, for example to provide the location of
    the bibliography database and the citation style definition

    ``` yaml
    bibliography: /bib/bibliography.tex
    csl: /bib/chicago-author-data.csl
    ```

In any case, path values for local file resources are interpreted either as
relative to the defining file, if specified as a *relative path*, or as relative
to the project root directory, if specified as an *absolute path*.

Consider the following project layout and file contents

``` txt
project
├── images
│   └── image.png
└── slides
    └── slide-deck.md
```

`slides/slide-deck.md` contains:

``` markdown
# First slide
![Project relative path](/images/image.png)
![Document relative path](../images/image.png)
```

Both image paths reference the same image file.

## Almost all variables

`compiletime-path-variables`
:   TODO

`css-dark-colors`
:   TODO

`css-light-colors`
:   TODO

`css-variables`
:   TODO

`decker-version`
:   TODO

`decker.base-dir`
:   TODO

`decker.base-dir`
:   TODO

`decker.filter.border`
:   TODO

`decker.filter.pretty`
:   TODO

`decker.filter.resources`
:   TODO

`deckId`
:   TODO

`draft`
:   TODO

`exclude-directories`
:   TODO

`extra-highlight-syntax`
:   TODO

`feedback.deck-id`
:   TODO

`highlight-style`
:   TODO

`mathjax-url`
:   TODO

`meta-data`
:   TODO

`no-index`
:   TODO

`palette.colors.dark`
:   TODO

`palette.colors.light`
:   TODO

`palette.contrast`
:   TODO

`publish.rsync.destination`
:   TODO

`publish.rsync.options`
:   TODO

`resource-pack`
:   TODO

`rsync-destination.host`
:   TODO

`rsync-destination.path`
:   TODO

`runtime-path-variables`
:   TODO

`short-links`
:   TODO

`static-resource-dirs`
:   TODO

`static-resources`
:   TODO

`subtitle`
:   TODO

`title`
:   TODO

`watch.exclude`
:   TODO

`whisper.base-dir`
:   TODO

`whisper.base-dir`
:   TODO

`whisper.lang`
:   TODO

`whisper.model`
:   TODO

`whisper.options.ffmpeg`
:   TODO

`write-back.enable`
:   TODO

`write-back.line-columns`
:   TODO

`write-back.line-wrap`
:   TODO

## Variables that can be defined in `decker.yaml`

`static-resources`
:   a list of files and directories that are copied to `public` without beeing
    referenced detectably in a presentation. Mostly used for HTML apps that are
    run inside of iFrame or `.htpasswd` files.

`exclude-directories`
:   a list of project directories that are not searched for `-deck.md` files.

## Variables that can be defined in the meta data section

## Variables for Reveal.js {#variables-revealjs}

Decker uses a modified version of the standard pandoc template for reveal.js
slide sets. Most if the variables used there are supported by decker.
Additionally, there are several Decker specific variables that control various
aspects of the generated slide sets.

`align-global`
:   default alignment for various slide elements (defaults to `left`)

`template.base-css` 🚧
:   the first CSS file that is loaded by the template (defaults to `''`)

`template.css` 🚧
:   a list of CSS files that is loaded after the default CSS files (defaults to
    `[]`)

`template.title-header`
:   a header image for the title slide (defaults to `''`)

`template.title-teaser`
:   an image that is placed below the title line on the title slide (defaults to
    `''`)

`template.affiliation-logo`
:   an imge that is placed above the affiliation information on the title slide
    (defaults to `''`)

`template.include-js` 🚧
:   a list of Javascript files that are included into the slide deck before
    Reveal.js is initialized (defaults to `[]`)

`style` 🚧

:   a list of CSS styles that are inserted into the HTML header (defaults to
    `[]`)

    For example, to set the background color of a all H2 header elements to red
    specify:

    ``` yaml
    style:
      - 'h2 { backgroundColor: #f00; }' 
    ```

`checkOverflow`
:   mark overrflowing slides with a red border (defaults to `false`)

`vertical-slides`
:   allow vertical slides (defaults to `true`)

### Dictionary

Decker has some content that can be adapted to the language of the presentation.
This is a work-in-progress and is currently used for quizzes.

The current default dictionary looks like this:

``` yaml
dictionary:
  de: 
    quiz:
      solution: Lösung zeigen
      input-placeholder: Eingeben und 'Enter'
      qmi-drag-hint: Objekte per Drag&Drop ziehen…
      qmi-drop-hint: …und hier in die richtige Kategorie einsortieren.
      ic-placeholder: Option auswählen…
  en:
    quiz:
      solution: Show Solution
      input-placeholder: Type and press 'Enter'
      qmi-drag-hint: Drag items from here…
      qmi-drop-hint: …and put them here into the correct category.
      ic-placeholder: Select option…
```

This dictionary can be partially or completely redefined by the user.

# Decker's Markdown

## Template Macros

Sometimes things get pretty repetitive. For example, required HTML for a custom
video embedding might look like this:

``` html
<video controls style="width: var(--slide-width); height: var(--slide-height);">
<source src="/videos/myvideo.mp4" type="video/mp4" />
<track kind="subtitles" label="Deutsch" srclang="de" src="/videos/myvideo.vtt" default />
</video>
```

Insertion of this stanza every time a video is embedded using this particular
method is tedious and error-prone.

Definition of a *template macro* in the meta data helps to control the
boilerplate.

``` yaml
templates:
  video: |
    <video controls style="width: var(--slide-width); height: var(--slide-height);">
    <source src="/videos/:(url).mp4" type="video/mp4" />
    <track kind="subtitles" label="Deutsch" srclang="de" src="/videos/:(url).vtt" default />
    </video>
```

Using this macro reduces the actual invocation to just:

``` markdown
[@video](/videos/myvideo)
```

### Link Macros

Link Macros can be invoked by standard Markdown link expressions.

Macro arguments are extracted from three different parts of the repurposed
Markdown link syntax, a space separated list of positional arguments directly
following the macro name in the square brackets, the *url* parameter specified
in the parenthesis and the optional *title* parameter.

``` markdown
    [macro_name arg1 arg2 ... argn](url "title")
```

The positional parameters can be referenced in the macro definition as `:(1)`,
`:(2)`, `:(3)`, ... (or `:(args)` for a sequence of all positional parameters),
the others as `:(url)` and `:(title)` respectively. The actual arguments are
inserted into the parsed Pandoc AST nodes of the definition by simple string
replacement.

For example, the following macro definition

``` yaml
templates:
  test: ":(1) :(2) :(3) :(url) :(title) :(args)"
```

invoked like this

``` markdown
[@test arg1 arg2 arg3 arg4](url "title")
```

will produce this text:

``` markdown
arg1 arg2 arg3 url title arg1 arg2 arg3 arg4
```

The *url* parameter is automatically URL-encoded by the Pandoc parser. This
might or might not be what you want.

### Code Block Macros

Code block macros can be invoked as standard Markdown code block expressions.

The macro name is given as the value of the `macro` attribute on the code block.
Macro arguments are extracted from two different parts of the repurposed
Markdown code block syntax. All other attribute value pairs are used as named
arguments and the classes are used as unnamed positional arguments. The contents
of the code block available as paramter `:(code)`

```` markdown
``` {macro="name" .arg1 .arg2 ... .argn param1="value2" param2="value2"}
code
```    
````

The positional parameters can be referenced in the macro definition as `:(1)`,
`:(2)`, `:(3)`, ... (or `:(args)` for a sequence of all positional parameters),
the attributes as `:(param1)` and `:(param")`. The code block content is
available as `:(code)`. `:(rnd-id)` is a for each macro invocation randomly
generated identifier that is suitable as an `id` value for HTML elements. The
arguments are inserted into the parsed Pandoc AST nodes of the definition by
simple string replacement.

For example, the following macro definition

``` yaml
templates:
  test: ":(1) :(2) :(3) :(param1) :(param2) :(args) :(code)"
```

invoked like this

```` markdown
``` {macro="test" .arg1 .arg2 .arg3 param1="value2" param2="value2"}
The code
```    
````

will produce this text:

``` markdown
arg1 arg2 arg3 value1 value2 arg1 arg2 arg3 The code
```

## Media handling

External media files like images or movies can be included in a presentation in
a variety of ways. The central mechanism is the standard Markdown inline image
tag as used by Pandoc.

``` markdown
![Image caption](/path/to/image.ext){width="100%"}
```

Several parameters describing the image can be encoded:

`[Image caption]`

:   If the image caption inside the square brackets `[]` is provided, the image
    will be set with the caption text right below the image. The caption text
    may contain further Markdown markup.

    A caption can also be specified by beginning the immediately following
    paragraph with the string `Caption:`. The rest of the paragraphs text is
    used as the caption.

`(/path/to/image.ext)`
:   The image itself is referenced with an URL inside the round brackets `()`. A
    relativ reference (as described in [RFC
    3986](https://tools.ietf.org/html/rfc3986#section-4.2)) here is interpreted
    as a path to a resource in the local file system that is either specified
    relative to the project's root directory or relative to the file containing
    the image tag.

`.ext`
:   The filename extension determines the media type of the image. Depending on
    the extension and media type the referenced resource may further be
    processed by decker to generate the final embedded media element.

`{width="100%"}`
:   The attributes annotation can be used to control various aspects of
    processing and presentation for the image, for example the width of the
    image relative to it's surrounding element (see [Local
    Paths](#local-paths)).

### Figures and captions

Embedded media will be rendered as a figure with caption if either

-   the square brackets of the image tag contain a caption text.

    ``` markdown
    ![This is the caption text.](some/image.png)
    ```

-   Or the image tag occurs on an otherwise empty paragraph followed directly by
    another paragraph that starts with the string `Caption:`. The second
    paragraph provides the text for the caption.

    ``` markdown
    ![](some/image.png)

    Caption: This is the caption text.
    ```

### Images

### Code blocks

Source code snippets can be included from an external file by either using the
image tag with a the `code` class or by using a Pandoc code block.

An example for a Javascript including image tag:

``` markdown
![Some Javascript code](source/code.js){.code .javascript} 
```

Standard Pandoc code blocks also work as expected:

```` markdown
``` javascript
let lork = () => {"lorgel"};
```
````

Syntax highlighting is either handled by Pandoc directly or using
[highlight.js](https://highlightjs.org). Two meta data variables control syntax
highlighting:

`highlightjs: <theme>`
:   If a theme is specified like this, highlight.js is used to perform syntax
    highlighting at load time. The theme `<theme>` is used. Pandoc does not
    process the contents of the code block.

`highlight-style: <theme>`
:   If `highlightjs` is not set, Pandoc processes the code block and emits
    highlighted spans for the content. The theme `<theme>` is used. If `<theme>`
    is invalid, the `monochrome` theme is used.

### Pdfs

### 3D polygonal models

### Iframes

### Videos

### Video streams

### Audio

### Graphs and diagrams

### Javascript ES6 modules

## Slide layout

## Whiteboard

## Audience Feedback

The audience of a deck can annotate slides with feedback. The feedback is
aggregated on a server and are visible by all audience members and the author.

The slide author can later choose to address the feedback by changing or
extending the information in the deck.

To enable this feature a deck must specify the URL of a Decker Engine server in
the meta data by setting the variable `decker-engine.base-url`. For example:

``` yaml
feedback:
  base-url: 'https://tramberend.bht-berlin.de/decker'
```

### Endpoints with authorization

There are two modes of operation depending on the deployment details of the
server, *authorized* and *public*.

If the server is running behind a proxy with Basic Authentication enabled,
questions can only be added if the user has been authenticated by the proxy.
Administrators are recognized automatically, no further authorization is
necessary.

The `de-api` endpoint works that way:

``` yaml
feedback:
  base-url: 'https://tramberend.bht-berlin.de/de-api'
```

### Public endpoints

If the server is publicly available without authentication, the user is assigned
a token that allows her to later delete or edit all questions that where added
using that token. The token can be entered by hand or stored in the browser's
local storage. Administrators need to authenticate with a username and a
password.

The `decker` endpoint works that way:

``` yaml
feedback:
  base-url: 'https://tramberend.bht-berlin.de/decker'
```

### Admistrators

Users that are authorized as administrators can answer, edit or delete all
questions in a set.

### Deck Identification

Decks are identified by their public URL. This can be problematic if a deck is
served locally, for example from
`http://localhost:8888/test/decks/engine-deck.html` during video recording, but
is supposed to show the questions on the published version. For this situation
the public URL of a deck can be set in the meta data.

``` yaml
feedback:
  deck-id: 'https://tramberend.bht-berlin.de/public/decker/test/decks/engine-deck.html'
```

If `decker-engine.deck-id` is specified, it overrides the actual deck URL as far
as deck identification for decker engine is concerned. The questions shown if
the deck is served locally will be the questions that where added to the
published deck.

## Quizzes

### Class definition

For each question type you can use either of the three tags to create quizzes

    .quiz-match-items, .quiz-mi, .qmi

    .quiz-multiple-choice, .quiz-mc, .qmc

    .quiz-insert-choices, .quiz-ic, .qic 

    .quiz-free-text, .quiz-ft, .qft

### Basic syntax

The quiz syntax is based on the markdown task list syntax. A markdown task list
looks like this

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

These questions generate quizzes where a user can drag and drop items to sort
them into "buckets".

This uses the Pandoc [definition list
syntax](https://pandoc.org/MANUAL.html#definition-lists).

You can provide distractor items (items not belonging to any bucket) or empty
buckets (no item belonging in those empty buckets) by using the exclamation mark
"!".

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

This will create a sort of blank text questions. If multiple items are provided
in the task list, they will be rendered as a drop down menu where the user can
click answers.

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

This will create a simple input field/text box where the user can write their
answer.

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

### Quiz Styling

The default style of quizzes includes decorative and interactive features. To
switch to a plain style, specify in YAML metadata, or use the `.plain` tag in
the question header.

``` .yaml
quiz: 
  style: plain
```

    # Question 1

    ## {.qmc .plain}

### Quiz Meta

Add a `YAML` code block to a question to provide meta information on the
specific question.

This is work in progress. Currently apart from `lang: de` or `lang: en` and quiz
style, it does not do anything. (21. Jul 2020)

    ``` {.yaml}
    lang: de
    score: 5
    category: FP
    lectureId: fp1
    topic: Functional Programming Introduction
    quiz:
      style: plain
    ```

## ThebeLab 🚧 {#thebelab}

## Sage

## GraphViz

## Gnuplot

# Hacking on Decker

## Conventions
