---
css: ./decker-users-guide.css
lang: en-US
title: Decker User's Guide
toc-title: Contents
---

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

The slide decks that Decker generates are [Reveal.js](https://revealjs.com)
presentations: single HTML pages that run in a web browser. Decker brings its own
copy of Reveal.js, so there is nothing to install. A slide has a fixed size of 1280
by 720 pixels and is scaled to fit the window. All keys below `reveal` in the meta
data go to Reveal.js unchanged, see [Reveal.js](#variables-revealjs) in the
configuration reference.

These keys work in every deck:

| Key                        | Action                                  |
|----------------------------|-----------------------------------------|
| `Space`, `N`               | Next slide or fragment                  |
| `Shift`+`Space`            | Previous slide or fragment              |
| Arrow keys                 | Move in that direction                  |
| `Home`, `End`              | First and last slide                    |
| `Esc`, `O`                 | Overview of all slides                  |
| `G`                        | Jump to a slide                         |
| `F`                        | Fullscreen                              |
| `B`, `.`                   | Pause and black out the screen          |
| `S`                        | Speaker view, if the `notes` plugin is on |
| `?`                        | Help, with all keys of the current deck |

Decker adds its own keys, and they take precedence. The Reveal.js documentation lists
`P`, `H` and `L` for navigation, but in a Decker deck `P` and `H` do nothing unless
pressed three times in quick succession, which toggles presenter mode and handout
mode. `L` is the laser pointer of the [whiteboard](#whiteboard). The help shows what
applies.

The button with the three bars opens Decker's menu with the list of slides, a link
to the index page, the search, the color mode, fullscreen and printing.

## Features

Beyond what Pandoc and Reveal.js provide, Decker offers:

- **Three kinds of output from one source.** A deck is built as a
  [presentation](#slides) and as a [handout](#notes-handouts-and-solutions), and
  [`decker pdf`](#decker-pdf) prints it to a PDF. Pages are ordinary web pages
  written in the same Markdown.
- **Slide layouts.** Columns, grids, popups and fragments, see [Slide
  layout](#slide-layout).
- **Media.** Images, videos, audio, PDFs, 3D models and iframes all use the Markdown
  image syntax, see [Media handling](#media-handling).
- **Reuse.** [Includes](#including-files), [template macros](#template-macros),
  [project-wide meta data](#meta-data) and [resource packs](#resource-packs) for the
  look of a whole course.
- **Interaction.** [Quizzes](#quizzes), [live quizzes](#live-quizzes), [live
  code](#live-code), a [whiteboard](#whiteboard) and [audience
  feedback](#audience-feedback).
- **Teaching material.** [Exam questions](#exam-questions) with a catalog and Moodle
  export, recording of presentations with [transcription](#decker-transcribe), a
  [search index](#decker-search-index) across all decks, and a
  [chatbot](#chatty) that answers questions about the course material.
- **A development server.** It rebuilds what has changed and reloads the open pages,
  see [Working on a project](#working-on-a-project).
- **Support for AI agents.** Decker explains itself to coding agents, see [Agent
  support](#agent-support).

## Installation

Prebuilt binaries for every release are available under
[Releases](https://github.com/decker-edu/decker/releases). The executable is
self-contained: all templates and support resources are bundled inside it.

### Homebrew (macOS and Linux)

On macOS and Linux the recommended way to install Decker is via
[Homebrew](https://brew.sh):

``` sh
> brew install decker-edu/decker/decker
```

This installs the latest release from the
[`decker-edu/homebrew-decker`](https://github.com/decker-edu/homebrew-decker) tap
together with the external tools Decker needs for its core features.

The tap offers three install tiers, each building on the previous one:

| Formula        | Installs                                            |
| -------------- | --------------------------------------------------- |
| `decker-naked` | Just the `decker` binary, no external tools         |
| `decker`       | Binary + the common tools (diagrams, mermaid, video)|
| `decker-full`  | Everything, including TeX Live and whisper.cpp      |

``` sh
> brew install decker-edu/decker/decker-naked  # binary only
> brew install decker-edu/decker/decker        # recommended default
> brew install decker-edu/decker/decker-full   # everything
```

Some optional features need additional programs. The `decker` tier prints a
caveats section explaining how to add them:

-   PDF export of decks needs Google Chrome (`brew install --cask google-chrome`)
-   Rendering embedded LaTeX needs a TeX distribution (`brew install texlive`)
-   Speech transcription needs `whisper.cpp` (`brew install whisper-cpp`)

The `decker-full` tier installs TeX Live and whisper.cpp automatically and leaves
only a Chrome/Chromium install on Linux to you.

Upgrade to a newer release with:

``` sh
> brew upgrade decker
```

### Manual installation

Download the binary for your platform from the releases page, make it executable
(`chmod +x decker` on macOS and Linux), and place it on your `PATH`. See the
project [`readme.md`](https://github.com/decker-edu/decker/blob/master/readme.md)
for platform-specific details, including Windows.

## Using Decker

Decker is a command line program that works on a *project*: a directory tree with
the configuration file `decker.yaml` at its root. You write Markdown in a text
editor of your choice. Decker turns it into HTML in the directory `public/`, and you
look at the result in a web browser. The fastest way to see this is the example
project. Run this in an empty directory:

``` sh
> decker example
> cd example
> decker --server --open
```

[`decker example`](#decker-example) writes the two files `decker.yaml` and
`minimal-deck.md`. `decker --server` builds them, starts a local web server and keeps
watching the project. `--open` opens `http://localhost:8888/index.html` in your
browser. The browser can be faster than the first build, so reload if the page does
not come up. Follow the link on the index page to the deck, then change something in
`minimal-deck.md` and save it. The deck reloads with your change. `^C` stops the
server.

Always look at the output through a web server, not by opening the HTML file
directly: decks load their code as JavaScript modules, which browsers do not load
from `file://` URLs.

The language defaults to German, which is also the language of the buttons and labels
that Decker generates. Put `lang: en` into `decker.yaml` for English.

## Creating a project

A project needs a file `decker.yaml` in its root directory, and it can be empty.
Without it, a build stops with:

``` txt
Error, file does not exist and no rule available:
  decker.yaml
```

Sources can be located anywhere below the root, and the name of a file decides what
it is, see the table in [Targets](#targets). The suffix counts, including the dash:
`intro-deck.md` is a deck, `deck.md` is not. Here is a small course:

``` txt
course
├── decker.yaml
├── syllabus-page.md
├── code
│   └── ...
├── lectures
│   ├── 01-intro-deck.md
│   └── img
│       └── logo.svg
└── static
    └── data.txt
```

`decker.yaml` holds what all documents have in common, see the [configuration
reference](#configuration-reference):

``` yaml {.decker-example file="decker.yaml" project="course"}
title: "Computer Graphics"
lang: en
exclude-directories:
  - code
static-resource-dirs:
  - static
```

`lectures/01-intro-deck.md` is the first deck. A new slide starts at each level-1
heading, see [Slides](#slides):

``` markdown {.decker-example file="lectures/01-intro-deck.md" project="course"}
---
title: Introduction
subtitle: The first lecture
---

# Welcome

![The logo of the course](img/logo.svg)

# Overview

- Light
- Geometry
```

``` xml {.decker-example file="lectures/img/logo.svg" project="course"}
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 1 1"><rect width="1" height="1"/></svg>
```

`syllabus-page.md` becomes an ordinary web page:

``` markdown {.decker-example file="syllabus-page.md" project="course"}
---
title: Syllabus
---

# Topics

The [first lecture](lectures/01-intro-deck.html) is about light.
```

``` txt {.decker-example file="static/data.txt" project="course"}
Measurements
```

Running `decker` in this project writes:

``` txt
public
├── index.html
├── syllabus-page.html
├── lectures
│   ├── 01-intro-deck.html
│   └── img
│       └── logo.svg
├── static
│   └── data.txt
└── support
    └── ...
```

Some things to know about the project structure:

- **The root.** You can run `decker` in any directory of the project. It goes up to
  the nearest directory that contains a `decker.yaml` or a `.git` directory, and uses
  that as the root. If that directory has no `decker.yaml`, the build fails with the
  message above. [`--project-dir`](#d-dir---project-dirdir) sets the root explicitly.
- **Generated files.** `public/` and `private/` are generated, and so is `chatty/` if
  you use [Chatty](#chatty). Do not edit them, and leave them out of version control.
- **Excluded directories.** Decker searches the whole tree, except for `public`,
  `chatty`, `dist`, `.git`, `.vscode`, `.stack-work` and whatever is listed in
  `exclude-directories`. Use this for directories with code or old material, or
  anything else that contains files with these suffixes.
- **Nested projects.** A `decker.yaml` further down does not stop the search. A
  project below another project, such as `example/` after running `decker example` in
  the root of a project, is also built as part of the outer project.
- **The index page.** `public/index.html` is generated with links to all decks and
  pages, unless there is an `index.md` in the root. Then it is built from that file,
  and the generated list moves to `public/index-generated.html`.
- **Media files.** Files that a document refers to, like `img/logo.svg`, are copied to
  `public/` automatically, to the same relative path. All other files have to be
  listed in `static-resources` or be located in one of the `static-resource-dirs`.
  A directory listed there has to exist, or the build fails.

For a project that an AI agent works on, see also [`decker
agent-docs`](#decker-agent-docs).

## Working on a project

While you are working on a project, keep the server running:

``` sh
> decker --server
```

Every change to a file in the project triggers a rebuild of whatever depends on it,
and open pages reload. New sources are picked up as well, and a change to `decker.yaml`
rebuilds the documents. A page in presenter mode does not reload, so that a
presentation is not interrupted. The server listens on port 8888 by default, use
[`-p`](#p-port---portport) to change that. Without a target, `--server` builds what
[`decker html`](#decker-html) builds, so name the targets if you need more:

``` sh
> decker --server html handouts
```

Without the server, these are the most common things to run:

| Command                                  | What it does                       |
|------------------------------------------|------------------------------------|
| `decker`                                 | Builds all decks and pages         |
| `decker public/lectures/01-intro-deck.html` | Builds just one file            |
| `decker handouts`                        | Builds the [handouts](#decker-handouts) |
| `decker pdf`                             | Builds the [PDFs](#decker-pdf), needs Chrome |
| `decker check`                           | Shows which [external programs](#decker-check) are found |
| `decker clean`                           | [Removes](#decker-clean) everything that was generated |

Decker reports problems that do not stop the build as warnings, and
[`--strict`](#diagnostics-and---strict) turns them into errors. A slide that is too
large for the window is not reported. Set `reveal.checkOverflow` to mark such slides,
see [Reveal.js](#variables-revealjs).

Removing a source does not remove its output. The document disappears from the
generated index page, but its HTML file stays in `public/`, and `decker publish`
copies it to the server. Run `decker clean` before publishing if you have removed or
renamed anything.

To exclude a document that is not finished yet from the search index, from `decker
publish -l` and from `decker chatty`, set `draft: true` in its front matter. It is
still built and can be viewed locally.

`public/support` contains Decker's runtime files. It is about 200 MB in size, and it
is the same for every project.

## Publishing

Decker can use a locally installed [Rsync](https://rsync.samba.org) to publish
the project to a remote location with the command

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

The default arguments of Rsync contain `--delete`, so the destination becomes a
*mirror* of the public directory: a file that exists only at the destination is
deleted. To change this, set the arguments in `external-tools.rsync.arguments`,
see [External tools](#external-tools).

What is published depends on the lecture flag `-l`, see [`decker
publish`](#decker-publish).

# Targets

``` sh
> decker [options] [target] ...
```

Decker uses [Shake](https://shakebuild.com) as its underlying build and
dependency tracking system. A target is one of the names in this chapter or a
file to be built. Only what is out of date gets built. Without a target, Decker
builds `html`. Several targets can be given at once:

``` sh
> decker decks handouts
```

These are the files that can be given as a target:

| Source                 | File target                                          |
|------------------------|------------------------------------------------------|
| `unit/x-deck.md`       | `public/unit/x-deck.html`, `public/unit/x-handout.html` |
| `unit/x-page.md`       | `public/unit/x-page.html`                            |
| `index.md`             | `public/index.html`                                  |
| `unit/x-quest.yaml`    | `private/unit/x-quest.html`                          |
| `unit/x-exam.yaml`     | `private/unit/x-exam.xml`                            |
| all `*-quest.yaml`     | `private/quest-catalog.html`, `private/quest-catalog.xml`, `public/exam-builder/questions.json` |

Most targets build the index page, which reads all questions. A question file
that does not parse therefore fails them, including `decks` and `pages`. Only
`static-files`, `search-index` and the `info` targets are not affected.

## `> decker html`

The default target. Builds all decks and pages, the question previews, the index
page and the support files. It does not build handouts or PDFs.

## `> decker decks`

Builds all decks (`*-deck.md`), the index page and the support files.

## `> decker pages`

Builds all pages (`*-page.md`) and the question previews, the index page and the
support files.

## `> decker handouts`

Builds the handouts of all decks (`public/*-handout.html`), the question
previews, the index page and the support files.

## `> decker questions`

Builds a preview for each question (`*-quest.yaml`) in `private/`, the index page
and the support files.

## `> decker catalog`

Builds `private/quest-catalog.html`, a browsable page of all questions.

## `> decker moodle-xml`

Builds Moodle XML for import into a question bank:

- `private/quest-catalog.xml` holds all questions, except for those with
  `Exam: false`.
- `private/<path>-exam.xml` holds the questions of the topics that are listed in
  `<path>-exam.yaml`. [`decker exam-builder`](#decker-exam-builder) writes these
  files.

## `> decker support`

Builds the index page, copies the static files and extracts Decker's runtime
files to `public/support`. The other build targets include this one.

## `> decker static-files`

Copies the directories listed in `static-resource-dirs` to `public/`.

## `> decker observed`

Meant to build only the pages that are open in a connected browser. No page is
ever registered as open at the moment, so this does the same as `decker support`.

## `> decker search-index`

Builds an inverted index over all decks and stores it in JSON in
`public/index.json`. The index can be used to implement incremental live-search
over all slides inside a Decker project. Pages are not included.

This may well be a little time consuming, so it is best called only right
before `decker publish`. `decker publish -l` builds the index itself.

Decks that have set `draft: true` or `lecture.status: draft`, or whose
(feedback) deck ids are in the global `no-index:` list, are removed from the
search index.

## `> decker info`

Prints the project, public, support and transient directories and where the
resources come from. With `--json`, it prints the project facts instead, see
[`decker info --json`](#decker-info---json).

## `> decker more-info`, `> decker even-more-info`

`decker more-info` also prints the global meta data, as Decker sees it after
merging all sources. `decker even-more-info` also prints all sources and targets.

## `> decker publish`

Builds the project and copies the directory `public/` to the configured
destination with Rsync (see [Publishing](#publishing)). `public/manifest.json`
lists all published files with their modification times and sizes. If no
destination is configured, Decker prints a message and exits with status 0.
Two modes are available, depending on whether the lecture flag `-l`
(`--lecture`) is given.

`> decker publish`

:   Builds all decks and pages and copies everything in `public/`. Nothing is
    filtered: **drafts are published**, and so are all files of earlier builds. To
    publish handouts and PDFs, build them first with `decker handouts` and `decker
    pdf`. The Chatty vector store is not touched.

`> decker publish -l`

:   *Lecture publishing.* Removes `public/`, `private/` and `chatty/` first and
    then builds only the search index and the decks and pages that are not drafts.
    A draft is a document with `draft: true` or `lecture.status: draft`. Handouts
    and PDFs are not built, so with the default Rsync arguments they disappear from
    the destination. The value of the `lecture.status` meta variable controls what
    is published:

    - `draft` --- the document is not published.
    - `done` --- the document is published completely.
    - `upcoming` --- the document is published, but [solutions](#solutions) are
      removed from the output.

    If a vector store is configured, it is synced as well, see
    [Vector store](#vector-store).

# Commands

A command must be the only target: `decker clean html` fails. Most commands do
not engage the global dependency checking and will not trigger rebuilds. The
exceptions are `pdf`, `chatty` and `exam-builder`, which build what they need
first.

## `> decker version`

Prints the versions of Decker, with branch, commit and build date, and of Pandoc.

## `> decker check`

Lists the programs defined in [`external-tools`](#external-tools), each with the
path where it was found, or `-` if it is missing. It only reports, and its exit
status is always 0.

## `> decker clean`

Removes the `public` and `private` directories and the generated `chatty`
directory.

## `> decker purge`

Does what `decker clean` does and also removes the project's transient directory,
where many things are cached during compilation, and all `.rendered-code`
directories. The transient directory is located in the temporary directory of the
system, `decker info` shows where. Expect increased compilation times after a
purge.

## `> decker example`

Writes a small example project to the directory `example` below the project root.
Files of an earlier example are overwritten. The current directory is the project
root if neither it nor any directory above it has a `decker.yaml` or `.git`. Inside
an existing project the example therefore becomes a part of that project, so use an
empty directory. To start exploring Decker:

``` sh
> decker example
> cd example
> decker --server --open
```

## `> decker serve`

Starts just the HTTP server for the existing `public/` directory, on the address
and port given by `-b` and `-p`. It builds nothing, does not watch any files and
does not reload browsers, so run a build first. The server also receives recordings
from the browser (see [`-P`](#p---poser)). It runs until it is interrupted, and
exits with status 7 if the port is in use.

To build, watch and serve at the same time, use `decker --server`.

## `> decker format`

Reads Decker Markdown from standard input and writes it to standard output in a
normalized form, for use from a text editor. It needs no project.

Decker's own syntax passes through unchanged, but be aware that this is a rewrite
by Pandoc: the keys of the front matter are sorted, list markers and rules are
normalized, footnotes move to the end, emoji codes become emoji and
`<span class="x">` becomes `[...]{.x}`.

## `> decker pdf`

Builds all decks (drafts as well) and compiles a PDF document for each one
(`public/*-deck.pdf`). It starts a headless Chrome browser and uses its printing
capabilities to do that. The decks are loaded from a server that Decker starts for
this purpose, so the port has to be free. Use `-p` if it is not.

Google Chrome has to be installed. On macOS it must be located in `/Applications`
or `/Users/<username>/Applications`, on Linux `chrome` has to be on the `PATH`.
`decker pdf` does not work on Windows: use the print button in the [menu](#menu) of the
deck. A message `thread killed` at the end of the run is normal, it is how the browser
gets stopped.

This may well be a little time consuming, so it is best called only right
before `decker publish`.

## `> decker crunch`

Uses `ffmpeg` to transcode raw video recordings of presentation sessions into MP4
files that use considerably less space on disk and in transit. A recording
`x-recording.webm` of the deck `x-deck.md` becomes `x-recording.mp4`. A recording in
several segments (`x-recording-0.webm`, `x-recording-1.webm`, ...) is transcoded
segment by segment and then combined into `x-recording.mp4`. Only what is older than
its source is redone, and the WebM files stay in place.

The segments are found by name: all files `x*.webm` next to the deck. Decks whose
names start with the name of another deck, such as `x-deck.md` and `x-extra-deck.md`,
must therefore be located in different directories, or the recording of `x-deck.md`
will contain the other one too.

## `> decker transcribe`

Uses `whisper.cpp` to transcribe the recording `x-recording.mp4` of each deck, so run
`decker crunch` first. The results are stored alongside the video files in `VTT`
format and are automatically available during playback:

- `x-recording-en.vtt`, translated to English if the recording is in another language.
- `x-recording-<lang>.vtt`, in the language of the recording.

Only what is older than the recording is redone. [`whisper.cpp`](https://github.com/ggml-org/whisper.cpp)
needs to be installed locally (e.g. via homebrew). The programs are defined by the
tools `mp4towav` and `whisper` in [`external-tools`](#external-tools). Configuration
meta data variables are:

``` yaml
# whisper.cpp transcription settings
whisper:
  model: /opt/whisper/ggml-large-v3.bin
  lang: de
```

The values shown are the defaults.

## `> decker chatty`

Builds the annotated Markdown for the project and syncs it to the configured
OpenAI vector store, see [Vector store](#vector-store). With `--prune-files`, it
also deletes the file objects in the OpenAI project that are not attached to the
store.

## `> decker exam-builder`

Opens a browser web app for browsing the project's exam questions and assembling
them into named exam collections. It first renders every `*-quest.yaml` question
into a catalog, then starts the development server and opens the app at
`http://localhost:8888/support/exam-builder.html` (use `-p` to change the port).

Questions are grouped by lecture and topic. Questions that share a lecture and
topic are treated as *variants* — the generated Moodle exam picks one of them at
random for each student — so collections refer to topics rather than to
individual variants. Pick the topics you want, give the collection a title, and
save it. Each saved collection is written as `exams/<title>-exam.yaml` and can be
reloaded or deleted from the app. The in-progress collection is kept in the
browser, so it survives a page reload.

The command keeps watching the project while the app is open: editing a
`*-quest.yaml` (or a saved `*-exam.yaml`) rebuilds the catalog and refreshes the
open app in place, preserving the current selection and scroll position.

## Agent support

Decker supports AI coding agents that author decks, pages and exam questions.
The agent gets its knowledge from the installed `decker` binary, so it always
matches the version that builds the project, and Decker reports content
problems itself, so the agent does not have to inspect the generated HTML.

### `> decker agent-guide [TOPIC]`

Prints the agent guide on standard output. Without a topic it prints the
index: the build-and-check loop, the source-to-target mapping, the diagnostic
codes, the commands an agent must not run, and the list of topics (`decks`,
`media`, `quizzes`, `pages`, `quests`, `config`, `templates`, plus any topics of the
configured resource pack). `decker agent-guide decks` prints one topic, `decker
agent-guide --all` prints everything.

Layout problems such as overflowing slides or broken columns are not reported
by the build. The `decks` topic tells the agent to check them visually: start
`decker serve -p PORT` in the background on a free port, open single slides
by id (`http://127.0.0.1:PORT/deck.html#/slide-id`) and take screenshots.
Decks do not work from `file://` URLs. If the port is in use, `decker serve`
exits with status 7.

The topics are Markdown files in `template/agent-guide/` of the resources. A
resource pack adds topics by shipping its own files there; a pack topic
replaces a Decker topic with the same name. The examples in the topics are
built by Decker's test suite, so they cannot drift from the implementation.

### `> decker info --json`

Prints project facts as one JSON document on standard output: every source
with its kind and build targets, the configured and resolved resource pack, a
curated part of the configuration (`lang`, `bibliography`, `csl`, `strict`,
`experiments`, `decker-plugins`, `page-features`, `codapi.server`,
`exclude-directories`, `static-resource-dirs`), the aggregate targets, the available fragment templates with their origin (`project`,
`pack` or `decker`) and the guide topics. Other meta data, for example chatty
or publishing settings, is never included. `decker info` without `--json` is
unchanged.

### `> decker agent-docs`

Installs two small pointer files that tell an agent to run `decker
agent-guide`:

- `.claude/skills/decker/SKILL.md` — a Claude Code skill.
- A section in `AGENTS.md`, delimited by `<!-- decker:begin -->` and
  `<!-- decker:end -->`. The file is created if it is missing; otherwise only
  the delimited section is replaced (or appended).

Both files are meant to be committed. Their content does not depend on the
Decker version, so re-running the command after an upgrade changes nothing
(reported as `Up to date`) and collaborators on different versions do not
fight over them. A copy of the guide written by earlier versions
(`.decker/agent-guide.md`) is deleted. Both pointer templates
(`template/skill.md`, `template/agents-md.md`) can be overridden by a resource
pack.

Options: `--no-skill` skips the skill, `--no-agents-md` skips `AGENTS.md`.

A project's `CLAUDE.md` or `AGENTS.md` should hold only project-specific
context (conventions, directory layout, naming rules, publishing target);
generic Decker knowledge comes from `decker agent-guide`.

### Diagnostics and `--strict`

Some mistakes do not fail a build, but produce wrong output. Decker reports
them as diagnostics with a stable code:

``` {.text}
[WARNING] unit/07-deck.md: Citeproc: citation Foo not found [citation-not-found]
```

| Code                            | Problem                                                       |
|---------------------------------|---------------------------------------------------------------|
| `citation-not-found`            | A citation key is not in the bibliography.                    |
| `citation-without-bibliography` | `[@key]` is used without a configured `bibliography`.         |
| `fragment-templates-disabled`   | A fragment template is invoked, but `experiments.fragment-templates` is off. |
| `quest-unknown-field`           | A question file has a field the parser ignores (a typo).      |
| `setext-heading`                | Hint: a dash line directly below text makes it a heading (see [Slides](#slides)). |
| `pandoc-*`                      | Any other pandoc warning, e.g. `pandoc-duplicate-note-reference`. |

The diagnostics of a target are stored with the build state and reported
again when a later build finds the target up to date. With `--strict` (or
`strict: true` in `decker.yaml`) a build that leaves any warning exits with
status 1; hints never fail a build. Strict mode does not apply while watching
or serving. `--diagnostics-json` additionally prints every diagnostic on
standard error as one JSON object per line, prefixed with
`decker-diagnostic: `.

# Options

## `-h`, `--help`

List all decker commands, targets and options.

## `-m key=value`, `--meta="key=value"`

Sets a [meta data](#meta-data) variable for this run. It overrides `decker.yaml`, but
not the meta data section of a document. The key can be compound, and the value must
not contain a `=`. `True` and `False` become boolean values, everything else is a
string:

``` sh
> decker -m lang=fr -m feedback.deck-id=intro html
```

## `-S`, `--server`

Runs the build, then serves the public directory via HTTP on
`http://localhost:8888` and watches for changes like `--watch`. Open pages reload
after every rebuild. Stop it with `^C`. Without a target it builds `html`.

To serve without building, use [`decker serve`](#decker-serve).

## `-p PORT`, `--port=PORT`

The port of the HTTP server, default 8888. If it is in use, the server exits
with status 7.

## `-b ADDRESS`, `--bind=ADDRESS`

The address the HTTP server listens on, default `127.0.0.1`. Use `0.0.0.0` to
make the server reachable from other machines, or from outside a container.
Decker still prints and opens `http://localhost:PORT` for the default address;
browsers fall back from IPv6 to `127.0.0.1`.

## `-w`, `--watch`

Runs the build, then watches for changes to files in the project and builds the
given targets again. Stop it with `^C`.

## `-n`, `--no-rebuild`

With `--server` or `--watch`: after a change, only reload the open pages and do not
build anything. The first build still runs.

## `-o`, `--open`

Opens `index.html` on the HTTP server in the default browser (`open` on macOS,
`xdg-open` on Linux, elsewhere the URL is printed). Use it with `--server`. The
browser opens before the first build is done and before the server runs, so the page
may have to be reloaded.

## `-P`, `--poser`

With a running server: as soon as a recording arrives from the browser, convert it
to MP4 next to the WebM file. The video stream is copied, so this is quick, but it
is meant for H.264 video. Without the option, only `decker crunch` creates MP4 files.

## `-l`, `--lecture`

Enables lecture publishing, see [`decker publish`](#decker-publish). The option also
works for builds: `decker -l html` builds documents with `lecture.status: upcoming`
without their solutions, and the next build without `-l` brings them back.

## `-e`, `--stderr`

Prints errors on standard error, turns off colors and keeps building the other
targets after an error. The exit status is still 1.

## `-t`, `--single-thread`

Builds with one thread instead of one per CPU core. This is the same as `-j1`.

## `-d DIR`, `--project-dir=DIR`

Change to `DIR` and use it as the project root, instead of searching upwards
from the current directory for `decker.yaml` (or `.git`). This is handy for
building a project that lives elsewhere, for example when running a development
build from the decker source tree:

``` {.sh}
stack run -- decker --project-dir=/path/to/other-project html
```

The directory is used verbatim, so the root is pinned even when it is nested
inside a larger Git repository.

## `--resource-base=URI`

Replace the default base resource pack (`exe:decker`) with the pack at `URI`.
The pack configured in `resource-pack` is still loaded on top of it. This is
meant for testing changes to the base pack without touching the installed
executable or the configured resource packs:

``` {.sh}
decker --resource-base=/path/to/modified-decker-pack html
```

`URI` takes the same forms as `resource-pack` (`exe:{name}`, `{path}` or
`{path}.zip`). The option is shorthand for `-m resource-base=URI`. Rebuild with
`decker clean` after switching back to the default base pack.

## `--strict`

Fail the build (exit status 1) if any content diagnostic is reported, see
[Diagnostics and `--strict`](#diagnostics-and---strict). Same as
`strict: true` in `decker.yaml`.

## `--diagnostics-json`

Also print each diagnostic as a JSON object on standard error, on a line
prefixed with `decker-diagnostic: `.

## `--json`

With `decker info`: print the project facts as JSON.

## `--all`

With `decker agent-guide`: print all topics.

## `--no-skill`, `--no-agents-md`

With `decker agent-docs`: do not write the Claude skill, or the section in
`AGENTS.md`.

## `--prune-files`

With `decker chatty`: after syncing, delete all assistant-purpose OpenAI file
objects that are not attached to the vector store.

## Options of Shake

`decker --help` also lists the options of Shake, the build system. Useful ones are
`-j[N]` for the number of threads, `-B` to build everything regardless of what is up
to date, `-k` to keep going after an error, `-V` for more and `-q` for less output.

# Resources

Decker needs more than its executable: templates for the build, support files
for the browser, and an example project. These files form a *resource pack*. The
base pack, `exe:decker` (`resource/decker/` in the Decker sources), is always
loaded. A project can select one more pack on top of it, which is how it gets a
different look and different defaults.

## Resource Packs

A pack is a directory, or a ZIP archive of one, with this layout:

| Path                      | Purpose                                              |
|---------------------------|------------------------------------------------------|
| `template/default.yaml`   | Default meta data. Required, but it can be empty     |
| `template/*.html`         | Pandoc templates (`deck.html`, `page.html`, `handout.html`, `index.html`) and the partials they include |
| `template/agent-guide/*.md` | Additional topics for `decker agent-guide`         |
| `support/`                | Files for the browser, copied to `public/support`    |
| `support/css/pack.css`    | Styles of the pack, see [Own stylesheets](#own-stylesheets) |
| `support/templates/*.html` | [Fragment templates](#fragment-templates)           |
| `example/`                | The project that `decker example` writes             |

### Selecting a pack

`resource-pack` in `decker.yaml`, or `-m resource-pack=...` for one build, selects
the pack. It has no effect in the front matter of a document.

| Value           | Pack                                                         |
|-----------------|--------------------------------------------------------------|
| `exe:NAME`      | A pack inside the executable, see [Bundled packs](#bundled-packs) |
| `PATH`          | A directory, relative to the project root                    |
| `PATH.zip`      | A ZIP archive, relative to the project root                  |

``` yaml
resource-pack: exe:mono
```

Without `exe:`, the value is a path: `resource-pack: mono` looks for a directory
`mono` in the project. If the pack or its `template/default.yaml` cannot be found,
the build fails with `file does not exist and no rule available`. Any other URL, such
as one starting with `https:`, is ignored without a message, and no pack is
loaded. `decker info` shows where the resources come from.

### How packs combine

The pack is searched first, then the base pack, file by file. So a pack only
contains what it changes:

- A template or partial of the pack replaces the one of the base pack. This also
  applies to a partial that a base template includes.
- A file below `support/` replaces the file with the same path, and all other files
  stay available.
- `template/default.yaml` is merged with the one of the base pack, see [Meta
  Data](#meta-data). Everything in `decker.yaml` takes precedence over it.

For testing changes to the base pack itself, `--resource-base=URI` replaces it. Such
a pack needs all templates, not just the changed ones.

### Writing a pack

This pack has a palette, a larger corner radius for blocks and a stylesheet:

``` yaml {.decker-example file="decker.yaml" project="pack"}
resource-pack: packs/course
exclude-directories:
  - packs
```

``` yaml {.decker-example file="packs/course/template/default.yaml" project="pack"}
palette:
  contrast: 0.4
css-variables:
  block-border-radius: 0.5em
```

``` css {.decker-example file="packs/course/support/css/pack.css" project="pack"}
.reveal h1 { text-transform: uppercase; }
```

``` markdown {.decker-example file="intro-deck.md" project="pack"}
# First slide
```

Things to keep in mind:

- List a pack inside the project in `exclude-directories`. Otherwise Decker searches
  it for sources like any other directory.
- A directory pack needs a `support` directory, and its path has to be relative. An
  installed Decker fails with `does not exist` or `InvalidRelDir` otherwise.
- **Use a directory if the pack has support files.** Decker reads the templates and
  `default.yaml` from a ZIP archive, but does not copy its `support` files to
  `public/support`.
- An installed Decker copies support files once. After changing them, or after
  selecting a different pack, run `decker clean`. Otherwise old files stay in
  `public/support`.

## Bundled packs

| Pack     | Slide size  | Templates      | Use                                       |
|----------|-------------|----------------|-------------------------------------------|
| `decker` | 1280 × 720  | its own        | The base pack, used without `resource-pack` |
| `mono`   | 1920 × 1080 | those of `decker` | Compact look, live code and live quizzes |
| `mario`  | 1280 × 720  | its own, except handouts | Lato, many math macros          |
| `tudo`   | 1280 × 720  | its own        | TU Dortmund: logo, captions, live quizzes, index with search |
| `nwburg` | 1244 × 700  | its own        | University of Würzburg: Roboto, banner on the title slide |
| `wburg`  | 1280 × 720  | its own        | Older version of `nwburg`. **Handouts cannot be built** |

`decker-plugins` and `page-features` are only read by the templates of `decker`, so
they work with `decker` and `mono`. The deck templates of the other packs load a
fixed list of plugins, and these keys have no effect there, although `decker info
--json` still reports them.

### mono

`mono` has no templates of its own. It changes the defaults and replaces the styles,
with the Recursive font and the [OKSolar](https://meat.io/oksolar) palette.

- **Plugins:** `chatty` and `quizzer` are on. `zoom`, `jingles`, `quiz`, `quiz-wue`,
  `feedback` and `notes` are off.
- **Quizzes stay static**, because `quiz-wue` is off. The questions appear, but do not
  react. Set `decker-plugins.quiz-wue: true` to use them.
- **Features:** `experiments.fragment-templates`, `page-features.codapi` and
  `page-features.math-module` are on.
- **Source links:** `experiments.add-document-path` is on, so every level 1 heading
  links to its source file. The link contains the absolute path on the build machine,
  and plain `decker publish` publishes it. Only `--lecture` leaves it out. Set the key
  to `false` for material that you publish.
- **Polls:** `poll-server` points to a server at BHT Berlin and `save-polls` is on.
  [Live quizzes](#live-quizzes) need your own `quizzer.url`.
- **Reveal.js:** `help` is off and `backgroundTransition` is `slide`.

The accents are in a different order than in the other packs: `.accent0` is magenta,
`.accent1` red, and so on up to `.accent7` violet. The named variables, such as
`--redish`, still mean what they say. Further additions:

| Addition                       | Effect                                       |
|--------------------------------|----------------------------------------------|
| `[text]{.accent1}`             | A badge. `.fg` colors only the text          |
| `[text]{.red}`, `.green`, `.blue`, `.black` | Colored text. `.bg` fills the background |
| `[link](url){.off}`            | A faded link that cannot be clicked          |
| `document-spacing` in `css-variables` | Scales the vertical spacing of pages and the index |

Its [fragment templates](#fragment-templates) are:

| Template                             | Invocation                              | Result                |
|--------------------------------------|-----------------------------------------|-----------------------|
| `wen`, `wde`                         | `[§wen](Article_Name "Display Text")`   | English or German Wikipedia link |
| `wp`                                 | `[§wp en](Article_Name "Display Text")` | Wikipedia link, language as argument (default `de`) |
| `wp-cc`                              | `[§wp-cc sa 4.0](File_Name.png)`        | Wikimedia file with a CC BY license note |
| `mdn`                                | `[§mdn](CSS/display "display")`         | Link to the German MDN Web Docs |
| `unit`, `subunit`, `assign`          | `[§unit](07 "Title")`                   | Links to `unit/07-deck.html`, `unit/07/07-deck.html` and `assignments/07-page.html` |
| `unit-nl`, `subunit-nl`, `assign-nl` | `[§unit-nl](07 "Title")`                | The same entries without a link |
| `nop`                                | `[§nop](x)`                             | Nothing               |

### mario

Own templates for decks, pages and the index. Text is centered, there is no progress
bar and no slide overview, and the transition is `none`. `highlightjs` is `xcode`,
`math.scale` is `0.9`, and `math.macros` defines `\R`, `\vec`, `\mat`, `\norm`,
`\diff`, `\pdiff` and more.

The deck template reads only `template.css` and `template.teaser`, an image on the
title slide. `template.deck.css` and the `template.*js` keys have no effect on decks.

### tudo

Own templates for all documents, in the design of TU Dortmund. There is no slide
overview, and the transition is `none`.

- **Title slide:** `includeTULogo` (defaults to `true`) shows the logo of the
  university, and `showDeckerLink` is on.
- **Live features:** [live captions](#live-captions) are on, with `caption-server` and
  `speech-recognition-language` set, and `quizzer.url` points to `quiz.jetzt`.
- **Recording:** `explain` sets the sizes of the recording and of the camera.
- **Styles:** `template.font` names a stylesheet below `support/fonts`, `lato` (the
  default) or `atkinson`. `template.page.css` and `template.index.css` add styles to pages and
  to the index.
- **Decks:** `hide-quizzes` hides all quizzes. The macro `[@deck](name)` links to the
  HTML and PDF version of a deck.
- **Index:** `index.manual` shows a manual, and `index.search` a search box.
  `index.searchShowsDeckTitle`, `index.searchShowsDeckSubtitle`,
  `index.listShowsDeckTitle` and `index.listShowsDeckSubtitle` choose what its results
  and the list of decks show. `index.links` also contains `pdf`.

### nwburg and wburg

Own templates, with `template.title-page.banner` as an image above the title. `nwburg`
also has `template.title-page.teaser-img` and `template.title-page.affiliation-logo`
as maps with the keys `file` and `height`, and for the teaser `margin`. In `wburg`,
they are plain paths.

`wburg` cannot build handouts, because its `handout.html` does not compile. It also has
a larger `example` directory, which `decker example` writes. Its pages and handouts do
not read the palette.

Other packs are maintained outside of Decker and can be used as a directory, like the
one in [Writing a pack](#writing-a-pack).

## Theming

A project changes the look of its documents with a palette, CSS variables and its
own stylesheets. All of this also works in `default.yaml` of a pack, and in the front
matter of a single document.

``` yaml {.decker-example file="decker.yaml" project="theme"}
palette:
  contrast: 0.4
  colors:
    light:
      - "#fbf7f0"
      - "#efe9dd"
      - "#d8d0c0"
      - "#a39b8b"
      - "#6f675a"
      - "#4f483c"
      - "#2e2a23"
      - "#1a1712"
      - "#b3261e"
      - "#c75a11"
      - "#a67c00"
      - "#3f7d20"
      - "#1f7a7a"
      - "#1d5fa8"
      - "#6a4c93"
      - "#7a5230"
css-variables:
  block-border-radius: 0.5em
css-light-colors:
  heading-color: "#1d5fa8"
template:
  deck:
    css:
      - /theme.css
```

``` css {.decker-example file="theme.css" project="theme"}
:root { --root-font-size: 32px; }
```

``` markdown {.decker-example file="intro-deck.md" project="theme"}
# First slide
```

### Palette

A palette has 16 colors. The first 8 are shades that run from the background to the
foreground color, the other 8 are accents. Use the `#rrggbb` form, anything else fails
the build with `sRGB24read: no parse`.

| Variables                         | Value                                     |
|-----------------------------------|-------------------------------------------|
| `--shade0` ... `--shade7`         | The shades of the current color mode      |
| `--accent0` ... `--accent7`       | The accents of the current color mode     |
| `--base00` ... `--base0F`         | All 16 colors under their Base16 names    |
| `--accent0-bg`, `--accent0-bbg`   | The color mixed with the background, once or twice |
| `--accent0-fg`, `--accent0-ffg`   | The color mixed with the foreground, once or twice |
| `--contrast`                      | `palette.contrast` as a percentage, the strength of these mixes |
| `--light-shade0`, `--dark-shade0`, ... | The colors of a specific mode        |

`palette.colors.light` and `palette.colors.dark` each replace the list of the pack, so
setting only `light` keeps the dark colors of the pack. If neither the project nor the
pack sets `light`, the built-in colors are used, even if `dark` is set.

In the base pack, the accents are expected in this order, because named variables and
classes are derived from it: red, orange, yellow, green, teal, blue, violet, brown.

### CSS variables

`css-variables` declares variables for both color modes, without the leading `--`.
`css-light-colors` and `css-dark-colors` do it for one mode only, and also replace
single colors of the palette. These declarations take precedence over everything in
`support/css/variables.css`, whose most useful variables are:

| Variable                                  | Default                       |
|-------------------------------------------|-------------------------------|
| `--background-color`, `--foreground-color` | `--shade0`, `--shade7`       |
| `--heading-color`                         | `--shade6`                    |
| `--primary-color`, `--secondary-color`    | `--accent5`, `--accent1`      |
| `--code-background-color`                 | `--shade1`                    |
| `--highlight-color`, `--hover-color`      | `--accent5-bg`                |
| `--focus-color`                           | `--accent1`                   |
| `--redish`, `--orangeish`, `--yellowish`, `--greenish`, `--tealish`, `--blueish`, `--violetish`, `--brownish` | `--accent0` ... `--accent7` |
| `--color-error`, `--color-alert`, `--color-warn`, `--color-success`, `--color-info` | The named colors |
| `--block-border-width`, `--block-border-radius`, `--block-outset` | Blocks |
| `--vertical-margin`                       | Space below block elements    |
| `--code-block-max-height`                 | `700px`                       |

**`css-variables` cannot change variables that a later stylesheet sets**, such as
`--root-font-size` in `deck.css`, which determines the size of all text on a slide. Use
an own stylesheet for them, as in the example above.

### Light and dark mode

A document follows the color mode of the system. The menu of a deck has a button to
change it for the current session, which puts the class `light` or `dark` on the
`html` element. Decker's stylesheets, and your own, can use these classes.

### Own stylesheets {#own-stylesheets}

Decks load their stylesheets in this order, and later ones win:

1. `support/css/variables.css`
2. The declarations from `palette`, `css-variables`, `css-light-colors` and
   `css-dark-colors`
3. `support/css/deck.css` and the styles of the plugins
4. `support/css/pack.css` of the pack
5. The files in `template.css`, then those in `template.deck.css`

Pages, handouts and the index follow the same scheme with their own stylesheets, and
without `template.deck.css`. A pack that needs other values for the variables of the
base pack replaces `support/css/variables.css`, as `mono` does. Such a file has to
define all of them.

### Plugins and tools

[`decker-plugins`](#plugins-and-page-features) switches deck plugins, and
[`external-tools`](#external-tools) defines the programs that Decker runs. Both can
be set in `decker.yaml` and in `default.yaml` of a pack. `decker info --json` shows
the plugins that result, and `decker check` the programs that are found.

## Versioning

The packs inside the executable always match its version. Packs loaded from a
directory or a ZIP archive carry no version, and Decker does not check them, so make
sure they fit the Decker version in use. Templates and variables change between
versions: compare a pack with `resource/decker/` when you upgrade.

# Meta Data

Meta data variables are specified in YAML format and can be defined in several
different places. In order of increasing precedence these are:

-   the `default.yaml` file of the base resource pack
-   the `default.yaml` file of the resource pack selected with `resource-pack`
-   the mandatory `decker.yaml` file that is read from the project's root
    directory
-   the files listed in `meta-data` in `decker.yaml`
-   the `-m key=value` options on the `decker` command line
-   the meta data sections of the slide source Markdown file
-   the files listed in `meta-data` in the meta data section of that file

Within a `meta-data` list, later files take precedence over earlier ones.

Values from several places are combined depending on their type. Maps are merged
key by key. Lists are joined, with the values of lower precedence first and without
duplicates, so a document adds to the `template.css` of `decker.yaml`. The lists
`palette.colors.light`, `palette.colors.dark`, `index.links` and all lists below
`external-tools` are replaced instead. For all other values, the one with the highest
precedence wins.

Meta data is hierarchical but most variables are defined at the top level. A
notable exception are variables that are used to set *local path* values (see
[Local paths](#local-paths)) in the slide template (see
[Appearance](#appearance)). These path values are located in the `template`
namespace. For example, the favicon of a document is provided in the
variable `template.favicon`.

Inside a YAML file or a YAML section of a file hierarchical meta variable values
are defined as follows:

``` yaml
template:
    favicon: /images/favicon.png
```

On the command line this can be specified as

``` sh
decker -m 'template.favicon=/images/favicon.png'
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
    bibliography: /bib/bibliography.bib
    csl: /bib/chicago-author-date.csl
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

## Configuration reference

This reference lists the keys that the Decker sources and the base resource pack
read, grouped by purpose. The defaults are in
`resource/decker/template/default.yaml`. Keys that only a bundled resource pack
reads are listed in [Keys of bundled resource packs](#pack-keys).

A complete `decker.yaml`, with the deck that uses it, might look like this:

``` yaml {.decker-example file="decker.yaml" project="config"}
title: "Computer Graphics"
lang: en
exclude-directories:
  - archive
template:
  favicon: img/logo.svg
reveal:
  width: 1600
  height: 900
index:
  links:
    - handout
decker-plugins:
  jingles: false
short-links:
  wiki: "https://en.wikipedia.org/wiki/@@"
```

``` markdown {.decker-example file="intro-deck.md" project="config"}
---
title: Introduction
subtitle: The first lecture
date: today
authors:
  - name: Ada Lovelace
    url: https://example.org/ada
    affiliation:
      name: Example University
      url: https://example.org
      logo:
        uri: /img/logo.svg
        height: 2rem
logos:
  - /img/logo.svg
---

# Reading

See [rendering](wiki:Rendering_(computer_graphics)).
```

``` xml {.decker-example file="img/logo.svg" project="config"}
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 1 1"><rect width="1" height="1"/></svg>
```

### Project and build

`resource-pack`
:   The resource pack to use, see [Resource Packs](#resource-packs). Only read
    from `decker.yaml` and the command line.

`resource-base`
:   URL of the base resource pack that `resource-pack` overrides. Defaults to
    `exe:decker`. Intended for testing modifications to the base pack, usually
    set with `--resource-base=URI`.

`meta-data`
:   A list of further YAML files to read, see the order above. These files can
    list further files.

`exclude-directories`
:   A list of directories that are not searched for sources and not watched.
    Each entry is a path from the project root: `archive` excludes `archive/`,
    but not `lectures/archive/`. `public`, `chatty`, `dist`, `.git`, `.vscode`
    and `.stack-work` are always excluded, as is every file or directory with a
    name that starts with a dot.

`static-resource-dirs`
:   A list of directories that are copied to `public` as a whole.

`static-resources`
:   A list of files and directories that are copied to `public`, although no
    document references them. Mostly used for HTML apps that run inside an iframe
    and for `.htpasswd` files.

`watch.exclude`
:   A list of glob patterns, matched against paths from the project root. Changes
    to matching files do not trigger a rebuild in `--watch` and `--server` mode.

`strict`
:   Like `--strict` on every build, see [Diagnostics and
    `--strict`](#diagnostics-and---strict).

`publish.rsync.destination`
:   The target of `decker publish`, for example `user@example.org:/var/www/course/`.
    To change the options of `rsync`, set `external-tools.rsync.arguments`.

`runtime-path-variables`
:   A list of keys whose values are paths of files needed when a document is
    shown. Decker copies such a file to `public` and replaces the value with the
    path relative to the document. The key can name a map or a list, then this
    applies to all strings below it. A value that is not an existing file stays
    as it is. The base pack lists `logos`, `affiliation`, `author`, `authors` and
    `teaser`. `template` always applies.

`compiletime-path-variables`
:   A list of keys whose values are paths of files needed during the build.
    Decker tracks them as dependencies. `csl`, `bibliography`, `meta-data`,
    `static-resource-dirs`, `static-resources` and `extra-highlight-syntax` always
    apply.

`write-back.enable`
:   Reformats the Markdown source files in place during the build (defaults to
    `false`).

`write-back.line-columns`
:   The line width for `write-back` (defaults to `80`).

`write-back.line-wrap`
:   The line wrapping for `write-back`: `auto` (the default), `none` or
    `preserve`.

`whisper.model`
:   The path of the model that `decker transcribe` uses.

`whisper.lang`
:   The language of the recordings for `decker transcribe` (defaults to `de`).

`experiments.fragment-templates`
:   Enables [fragment templates](#fragment-templates) (defaults to `false`).

`experiments.add-document-path`
:   Adds a link to the source file to every level 1 heading (defaults to
    `false`).

`editor.link-prefix`
:   The start of the URLs that link to source files, followed by the absolute
    path of the file (defaults to `zed://file`). Used by
    `experiments.add-document-path` and by the previews of exam questions. For VS
    Code use `vscode://file`.

### Documents and title slide

`title`, `subtitle`
:   The title and the subtitle. `title` defaults to `false`, which omits it.

`date`
:   The date on the title slide. The value `today` is replaced with the date on
    which the deck is shown.

`author`, `authors`
:   One author, or a list of them. Use only one of these keys. An author is a
    string, or a map with the keys `name`, `url`, `photo` (a path) and
    `affiliation`.

`affiliation`
:   A string, or a map with the keys `name`, `url` and `logo` (an image).

`teaser`
:   An image on the title slide.

`logos`
:   A list of images on the title slide.

`copyright`
:   A string, or a map with the keys `type` and `url`.

`showDeckerLink`
:   Shows a link to Decker on the title slide.

`lang`
:   The language of the document (defaults to `de`). It selects the entries of
    the [dictionary](#dictionary) and is set as the HTML `lang` attribute.

`dir`
:   The text direction, set as the HTML `dir` attribute.

`watermark`
:   Text or HTML to show as a watermark on the deck.

`keywords`, `description-meta`
:   The content of the HTML `meta` tags `keywords` and `description`.

`header-includes`, `include-before`, `include-after`
:   Raw content for the start and the end of the HTML document.

`bibliography`, `csl`
:   The bibliography file and the citation style. Without `csl`, a default style
    is used.

`deckId`
:   The identifier of the document in the generated lists `decks.by-id` and
    `pages.by-id`.

`draft`
:   Marks a document as a draft. It is left out of the search index, of the
    selection of `decker publish --lecture` and of `decker chatty`. It is still
    built and still appears on a generated index page.

`lecture.status`
:   The value `draft` is the same as `draft: true`. The value `upcoming` removes
    slides and boxes with the class `.solution` from the document for `decker
    publish --lecture`.

`no-index`
:   A list of values of `feedback.deck-id`. Documents with one of these values are
    treated like drafts for the search index and for `decker publish --lecture`.

`short-links`
:   Defines URL schemes for links. In the template, `@@` is replaced with the rest
    of the link, and `@@@` with the rest of the link in URL encoding. With the
    configuration below, `[a](wiki:Rendering)` links to
    `https://en.wikipedia.org/wiki/Rendering`. A scheme can have several
    templates, one of which is selected in `short-links.bind`:

    ``` yaml
    short-links:
      wiki: "https://en.wikipedia.org/wiki/@@"
      bind:
        code: github
      code:
        github: "https://github.com/decker-edu/@@"
        gitlab: "https://gitlab.com/decker-edu/@@"
    ```

An image, as used by `logo`, `teaser` and `logos`, is a path or a map with the keys
`uri`, `alt`, `width` and `height`.

### Appearance

`template.favicon`
:   The icon of all generated documents.

`template.css`
:   A list of CSS files for all generated documents.

`template.js`, `template.js-module`
:   Lists of scripts, classic and ES6 modules, for decks and pages.

`template.deck.css`, `template.deck.js`, `template.deck.js-module`
:   Lists of CSS files and scripts for decks only.

`template.page.js`, `template.page.js-module`
:   Lists of scripts for pages only. The base pack has no `template.page.css`.

`palette.colors.light`, `palette.colors.dark`
:   Lists of 16 colors. The first 8 run from the background to the foreground color
    and become the CSS variables `--shade0` to `--shade7`, the other 8 become
    `--accent0` to `--accent7`. Both also have the names `--base00` to `--base0F`.
    If only `light` is set it is used for both modes. If `light` is missing, the
    built-in colors are used for both modes.

`palette.contrast`
:   A number from 0 to 1 that becomes the CSS variable `--contrast` as a percentage
    (defaults to `0.25`).

`css-variables`
:   A map from names to values, which become CSS variables: `block-border-radius: 1em`
    declares `--block-border-radius: 1em;`. See [Theming](#theming).

`css-light-colors`, `css-dark-colors`
:   Maps like `css-variables` for one color mode. They take precedence over the
    colors derived from the palette.

`highlight-style`
:   The name of a Pandoc highlight style for code blocks (defaults to
    `monochrome`).

`highlightjs`
:   Uses Highlight.js instead of Pandoc to highlight code blocks in decks. The
    value is the name of a theme in `support/vendor/css`: `atom-one-dark`,
    `atom-one-light`, `atom-one`, `decker`, `solarized-dark`, `solarized-light`,
    `xcode` or `zenburn`. `highlight-style` is then ignored.

`extra-highlight-syntax`
:   A map from language names to Skylighting syntax definition files.

`math.scale`
:   The scale of formulas in decks (defaults to `1`).

`math.macros`
:   A map from names to MathJax macro definitions, for decks.

`math.linebreaks`
:   Set to `false` to disable automatic line breaks in inline formulas.

`chart.defaults`
:   Options that are merged into `Chart.defaults` of Chart.js.

`chart.colors`
:   A list of colors for [charts](#charts), as `#rrggbb`. Without it, Chart.js chooses.

### Index pages

`index.links`
:   The links shown next to every deck on an index page, any of `a11y`, `handout`,
    `pdf` and `presenter` (defaults to `a11y` and `handout`). Index pages
    add them to the links that `index.selector` matches.

`index.selector`
:   A CSS selector for the links to decks (defaults to `a[href$='-deck.html']`).

`index.progress`
:   Shows how far a reader has got in each deck (defaults to `true`). As a map, the
    key `insert` places the indicator relative to the link: `after` (the default),
    `before` or `replace`.

### Plugins and page features

`decker-plugins`
:   Switches the optional deck plugins on and off, with the templates of the base
    pack and `mono`. Other [bundled packs](#bundled-packs) ignore it. `mono` has other
    defaults, and `decker info --json` shows the result.

    | Key        | Default | Purpose                                        |
    |------------|---------|------------------------------------------------|
    | `zoom`     | `true`  | Zooming into a slide                           |
    | `jingles`  | `true`  | Jingles that are played with a keyboard shortcut |
    | `quiz-wue` | `true`  | [Quizzes](#quizzes)                            |
    | `quiz`     | `true`  | An old implementation. It is loaded, but not started, and has no effect |
    | `feedback` | `true`  | [Audience feedback](#audience-feedback)        |
    | `notes`    | `true`  | Speaker view                                   |
    | `chatty`   | `false` | Chat assistant, see [Chatty](#chatty)          |
    | `quizzer`  | `false` | [Live quizzes](#live-quizzes)                  |

    The other plugins are always loaded, except for the highlight plugin, which
    needs `highlightjs`, and [live captions](#live-captions), which need
    `live-captions`.

`reveal-plugins`
:   A list of additional Reveal.js plugins, each a map with the name of the plugin
    in `name` and the path of its ES6 module in `url`.

`page-features.codapi`
:   Works like `decker-plugins`, only with these packs. Loads [Codapi](#live-code) on pages and index pages (defaults to `false`). Decks
    always load it.

`page-features.math-module`
:   Works like `decker-plugins`, only with these packs. Loads MathJax on pages and index pages through `support/js/page-math.js`
    instead of the inline configuration (defaults to `false`).

`codapi.server`
:   The URL of your own Codapi server, see [Running your own
    server](#running-your-own-server).

Plugins read their own options from a key with the name of the plugin, for example
`zoom`, [`whiteboard`](#whiteboard) or [`explain`](#recording).

### Reveal.js {#variables-revealjs}

All keys below `reveal` are passed to Reveal.js as they are, so all of its
[configuration options](https://revealjs.com/config/) are available. These are the
most important ones:

`reveal.width`, `reveal.height`
:   The size of a slide, which is scaled to the window (defaults to 1280 and 720).

`reveal.margin`
:   The empty space around the slide, as a fraction of its size (defaults to
    `0.05`).

`reveal.transition`
:   The slide transition (defaults to `slide`).

`reveal.checkOverflow`
:   Marks slides that are too high with a dashed red border when they are shown and
    logs a warning to the browser console (defaults to `false`).

### External tools

`external-tools` defines the programs that Decker runs. The default definitions
are `rsync`, `gnuplot`, `plantuml`, `chromeheadless`, `chrome`, `mermaid`, `dot`,
`d2`, `pdf2svg`, `pdflatex`, `mp4towav`, `whisper`, `tomp4-copy`, `tomp4-transcode`
and `tomp4-concat`. A definition has these keys:

`command`
:   The program to run.

`arguments`
:   The list of arguments. `${input}`, `${output}` and `${tmpdir}` are replaced with
    the input file, the output file and a directory for temporary files.

`pipe`
:   If `true`, the input file is passed on standard input and standard output is
    written to the output file.

`help`
:   A hint that is shown when the program cannot be run.

If a tool has different definitions per operating system, they are located below
the keys `linux`, `macos` and `windows`, and take precedence over a definition
without one. To change a default, set only what differs. A list replaces the
default list:

``` yaml
external-tools:
  rsync:
    arguments:
      - "--recursive"
      - "--delete"
      - "${input}"
      - "${output}"
```

`decker check` shows which of these programs can be found.

### Dictionary {#dictionary}

Decker has some content that depends on the language of the document, currently
the labels of quizzes and exam questions. The dictionary has one entry per
language below `dictionary`, and `lang` selects one of them. Missing entries are
taken from `en`. The complete default is in `default.yaml`, and can be redefined
partially or completely. This is a part of it:

``` yaml
dictionary:
  en:
    quiz:
      solution: Show Solution
      reset-button: Reset
      input-placeholder: Type and press 'Enter'
      qmi-drag-hint: Drag items from here…
      qmi-drop-hint: …and put them here into the correct category.
      ic-placeholder: Select option…
    exam:
      solution: Solution
      points: Points
```

### Keys of bundled resource packs {#pack-keys}

These keys have no effect with other packs, see [Bundled packs](#bundled-packs):

| Key                                            | Packs                            |
|------------------------------------------------|----------------------------------|
| `template.base-css`                            | `mario`, `nwburg`, `wburg`       |
| `template.include-js`                          | `wburg`                          |
| `template.title-page.*`                        | `nwburg`, `wburg`                |
| `template.index-page.banner`                   | `mario`, `nwburg`, `tudo`, `wburg` |
| `template.page.css`, `template.index.css`, `template.font`, `includeTULogo`, `hide-quizzes`, `index.manual`, `index.search`, `index.searchShowsDeckTitle`, `index.searchShowsDeckSubtitle`, `index.listShowsDeckTitle`, `index.listShowsDeckSubtitle` | `tudo` |
| `template.teaser`                              | `mario`                          |

### Generated values

Decker sets these keys during the build, for use in templates. Do not set them
yourself.

`build-time`, `decker-meta-url`, `decker-support-dir`
:   The time of the build, the file that holds the meta data of the document as
    JSON, and the relative path to the support directory.

`decker.base-dir`, `decker.doc-path`, `decker.filter.resources`, `decker.filter.links`
:   The directory and the path of the document, and the local resources and links
    that it uses.

`css-declarations`, `css-light-color-declarations`, `css-dark-color-declarations`
:   CSS variable declarations.

`decks.by-*`, `pages.by-*`, `quests.by-*`
:   The lists of all documents, for index pages. For example, `decks.by-title` lists
    all decks by title.

Two keys help when debugging the filters: `decker.filter.border` draws a border
around generated media elements, and `decker.filter.pretty` indents their HTML.

# Decker's Markdown

## Slides {#slides}

A new slide starts at every level-1 heading (`# ...`) and at every horizontal
rule, a line of three or more dashes with blank lines around it. A rule
directly followed by a heading counts as one slide break. A heading with the
class `.sub` (`# Title {.sub}`) makes a vertical sub-slide below the previous
slide. Level-2 headings inside a slide start blocks, see [Slide
layout](#slide-layout).

A line of dashes directly below a line of text, without a blank line in
between, is not a rule: Pandoc reads it as a setext heading and turns the text
into a level-2 heading. Decker reports a `setext-heading` hint for it:

``` {.markdown .decker-example file="setext-deck.md" expect="setext-heading"}
Some text
---
```

Always put a blank line before a slide separator.

## Including files

A paragraph that consists only of an include link is replaced by the content
of another Markdown file:

``` markdown
[:include](../topic/intro.md)
```

- The link must be the only content of its paragraph.
- A relative path is resolved against the including file, a path starting
  with `/` against the project root.
- The YAML front matter of the included file is ignored; only its body is
  inserted.
- Includes nest. A missing file is a build error.

Markdown files without one of the suffixes `-deck.md` or `-page.md` produce no
output of their own and can only be used this way.

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
[@video](myvideo)
```

The form of the definition decides how it is inserted. A template in
`decker.yaml` is a plain string, and it is inserted as raw HTML, without any
Markdown processing. A template in the front matter of a document is read as
Markdown first.

### Parameters

`:(name)` in a template is replaced by the argument of that name.

- A parameter without an argument is replaced by its own name: `:(3)` becomes
  `3` if the macro got only two positional arguments.
- `:(a|b)` takes the first of the names that has an argument. If none has one,
  it is replaced by the last name, which makes this a default value:
  `:(lang|en)` is `en` if there is no argument `lang`.
- A link macro always has the arguments `url` and `title`, even if they are
  empty. `:(title|url)` is empty for a link without a title.

### Link Macros

Link Macros can be invoked by standard Markdown link expressions.

The macro name is prefixed with `@`. Macro arguments are extracted from three
different parts of the repurposed Markdown link syntax, a space separated list of
positional arguments directly following the macro name in the square brackets, the *url* parameter specified
in the parenthesis and the optional *title* parameter.

``` markdown
    [@macro_name arg1 arg2 ... argn](url "title")
```

The positional parameters can be referenced in the macro definition as `:(1)`,
`:(2)`, `:(3)`, ... (or `:(args)` for a sequence of all positional parameters),
the others as `:(url)` and `:(title)` respectively. The actual arguments are
inserted into the parsed Pandoc AST nodes of the definition by simple string
replacement.

Instead of the `@` prefix, the name can end with a colon:

``` markdown
    [macro_name: arg1 arg2 ... argn](url "title")
```

Use this form in documents with a bibliography. There, Pandoc reads `[@name ...]`
as a citation before the macro is expanded. The macro stays unexpanded and
Decker reports `citation-not-found`.

A link whose name has no template remains a normal link.

For example, the following macro definition

``` {.yaml .decker-example file="decker.yaml" project="link-macro"}
templates:
  test: ":(1) :(2) :(3) :(url) :(title) :(args)"
```

invoked like this

``` {.markdown .decker-example file="link-macro-deck.md" project="link-macro"}
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
of the code block are available as parameter `:(code)`.

```` markdown
``` {macro="name" .arg1 .arg2 ... .argn param1="value1" param2="value2"}
code
```    
````

The positional parameters can be referenced in the macro definition as `:(1)`,
`:(2)`, `:(3)`, ... (or `:(args)` for a sequence of all positional parameters),
the attributes as `:(param1)` and `:(param2)`. The code block content is
available as `:(code)`. `:(rnd-id)` is a for each macro invocation randomly
generated identifier that is suitable as an `id` value for HTML elements. The
arguments are inserted into the parsed Pandoc AST nodes of the definition by
simple string replacement.

`:(attribs)` holds all attributes as `key="value"` pairs. `:(id)` is the id of the
code block or, without one, the same value as `:(rnd-id)`. The prefix `data-` is
removed from attribute names.

For example, the following macro definition

``` {.yaml .decker-example file="decker.yaml" project="block-macro"}
templates:
  test: ":(1) :(2) :(3) :(param1) :(param2) :(args) :(code)"
```

invoked like this

```` {.markdown .decker-example file="block-macro-deck.md" project="block-macro"}
``` {macro="test" .arg1 .arg2 .arg3 param1="value1" param2="value2"}
The code
```    
````

will produce this text:

``` markdown
arg1 arg2 arg3 value1 value2 arg1 arg2 arg3 The code
```

## Built-in macros

These macros are links whose text starts with a colon. They work in every
document type.

| Macro                 | Result                                                     |
|-----------------------|------------------------------------------------------------|
| `[:fa](name)`         | The Font Awesome solid icon `name`, the same as `[:fas]`   |
| `[:far](name)`        | The regular icon `name`                                    |
| `[:fab](name)`        | The brand icon `name`                                      |
| `[:hspace](width)`    | Horizontal space of a CSS width                            |
| `[:vspace](height)`   | Vertical space of a CSS height                             |
| `[:meta](key)`        | The value of a meta data key, struck through if it is unset |

The icon name has no `fa-` prefix. Classes and a `style` attribute on the link are
passed on to the icon:

``` {.markdown .decker-example file="macros-deck.md"}
---
title: Macros
---

# Icons and space

[:fa](ghost){style="color: red;"} [:fab](github)

[:vspace](2em)

This deck is called [:meta](title).
```

## Fragment templates

Fragment templates are HTML snippets, written as Pandoc doctemplates, that are
expanded from special links and code blocks. They are an experiment and have
to be enabled:

``` yaml
experiments:
  fragment-templates: true
```

A link whose text starts with `§` (or whose first word ends with a dash), and
a code block with a class ending in a dash, invoke the template of that name:

```` markdown
[§wikipedia en](Haskell_(programming_language) "Haskell")

[wikipedia- en](Lambda_calculus "Lambda calculus")

``` {.live-code- language="Haskell"}
main = print 42
```
````

If the experiment is off, the invocations remain plain links and code blocks,
and Decker reports `fragment-templates-disabled`.

The template `<name>.html` is searched in this order:

1. `<name>.html` relative to the directory of the document
   (`decker.base-dir`),
2. `templates/<name>.html` in the project,
3. `support/templates/<name>.html` in the resource pack, then in Decker.

An unknown name is a build error. The template receives these variables:

| Variable                  | Value                                                  |
|---------------------------|--------------------------------------------------------|
| `url`, `title`            | Link target and title (link form)                      |
| `arg1`, `arg2`, …, `args` | Words after the name in the link text; `argn` = count  |
| `id`                      | The element id, or a random id                         |
| `class1`, …, `classes`    | Classes of the link or code block                      |
| `<key>`, `attribs`        | Key/value attributes (`data-` prefix removed)          |
| `code`, `codeEsc`         | Code block content, raw and HTML-escaped (code form)   |
| `rnd-id`, `caption`       | Random id and `caption` attribute (code form)          |
| `meta`                    | The document's meta data                               |

Decker ships `wikipedia`, `live-code`, `live-code-server`,
`live-code-prelude`, `grid`, `finegrid` and `canvas` (see [Live
code](#live-code)); resource packs can add more. `decker info --json` lists
the templates available in a project.

## Media handling

All media use the Markdown image syntax. Decker chooses what to generate from the
URL and the classes:

``` markdown
![Caption](path/to/file.ext "Label"){#id .class width="50%"}
```

| Media                             | Extension or scheme                       | Class          |
|-----------------------------------|-------------------------------------------|----------------|
| [Image](#images)                  | `jpg` `jpeg` `png` `gif` `tif` `tiff` `bmp` `svg`, and everything unknown | `.image` |
| [Video](#videos)                  | `mp4` `mov` `ogg` `avi` `webm`            | `.video`       |
| [Iframe](#iframes)                | `html` `htm` `php`                        | `.iframe`      |
| [PDF](#pdfs)                      | `pdf`                                     | `.pdf`         |
| [3D model](#3d-polygonal-models)   | `off` `obj` `stl` `ply` `pmp` `xyz` `agi` | `.mview`       |
| [3D model](#3d-polygonal-models)   | `gltf` `glb`                              | `.modelviewer` |
| [GeoGebra](#geogebra)             | `ggb`                                     | `.geogebra`    |
| [Audio](#audio)                   | `mp3` `aiff` `wav`                        | `.audio`       |
| [Video stream](#video-streams)    | `youtube:` `vimeo:` `twitch:`             |                |

If more than one row matches, the upper one wins. A class overrides the extension
of a lower row, so `![](x.pdf){.iframe}` shows a PDF in an iframe. A URL without a known extension
is an image, which is why a web page needs the class: `![](https://example.com/){.iframe}`.

These classes come first and have their own meaning:

| Class                    | Works with                  | Effect                                      |
|--------------------------|-----------------------------|---------------------------------------------|
| `.raw`                   | everything                  | A plain `<img>`, see [Images](#images)      |
| `.code`                  | everything                  | The file as source code, see [Code blocks](#code-blocks) |
| `.render`                | `dot` `gnuplot` `tex` `plantuml` `d2` `mmd` | A diagram, see [Graphs and diagrams](#graphs-and-diagrams) |
| `.run`                   | `js`                        | A module, see [Javascript ES6 modules](#javascript-es6-modules) |
| `.embed`                 | `svg`                       | The SVG inside the HTML, see [Images](#images) |

**Paths.** A local path is resolved as described in [Local paths](#local-paths).
Decker copies the file to the same relative path below `public/`, and a missing file
is a build error. Only the file itself is copied. Whatever it loads, such as the
images of an iframe page, has to be listed in `static-resources` or located in one
of the `static-resource-dirs`. Those are copied by `decker html`, `decker decks` and
the other aggregate targets, but not when you build a single file. Remote URLs are
passed on without a check. A query string stays part of the URL:
`![](frame.html?x=1)` copies `frame.html`.

### Figures and captions

A medium with a caption is rendered as a figure. The caption comes from the square
brackets, or from a paragraph directly below it that starts with `Caption:`. The
paragraph wins if there are both. It also works for code blocks and block quotes.

``` markdown
![This is the caption text.](some/image.png)

![](some/image.png)

Caption: This is the caption text, with *Markdown*.
```

The title in the URL part (`"Label"`) is the `aria-label` for screen readers. Without
a title, the text of the caption is used. Iframes also use it as their `title`.

### Size and attributes

`width`, `height`
:   CSS lengths. Without them, the medium fills the width of its block. With only
    one of them, images and videos keep their aspect ratio.

`w:h`, `aspect-ratio`
:   The aspect ratio of an iframe, PDF, 3D model, GeoGebra applet or stream, for
    example `w:h="4/3"`, with a default of `16/9`. They have no effect if both `width`
    and `height` are set. On images and videos, they only matter in a
    [row](#rows-of-media).

`align`
:   `left` or `right`, for images. Everything is centered by default, and other media
    ignore this attribute.

`css:property`
:   A CSS property for the container, for example `css:border="1px solid red"`.

`style`
:   Goes to the container, for iframes, PDFs, 3D models and GeoGebra applets to the
    element itself.

The `id` and the classes go to the container, except that the classes of
[fragments](#fragments) go to the figure. So `![](a.png){.fragment}` works as you
would expect. All other attributes turn into `data-*` attributes.

### Images

An image alone in a paragraph is a block, and an image among other text is part of
the line. There, it needs a `height` to fit into the line:

``` {.markdown .decker-example file="images-deck.md" project="images"}
---
title: Images
---

# Images

![A figure](dot.svg){width="30%" align="right"}

An icon ![](dot.svg){height="1em"} in the text.
```

``` {.xml .decker-example file="dot.svg" project="images"}
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 10 10"><circle cx="5" cy="5" r="4"/></svg>
```

`.embed` inserts an SVG file into the HTML instead of loading it, so that its elements
can be styled from the document. Elements in it with the class `fragment` become
[fragments](#fragments).

`.raw` skips all of this. The result is an `<img>` without a figure or a size, and
attributes such as `width` or `style` stay on it. The caption is its `alt` text.

### Rows of media

A line block with one medium per line is a row. The columns are as wide as they need
to be for all media to have the same height. Decker reads the aspect ratio of images
from the file and of videos with `ffprobe`. For everything else, and if that fails,
it is `16/9`, unless `w:h` says otherwise:

``` markdown
| ![First](a.png)
| ![Second](b.mp4){.controls}
| ![Third](model.off){w:h="1/1"}

Caption: A caption for the whole row.
```

### Slide backgrounds

The last image in a level-1 heading becomes the background of the slide:

``` markdown
# Title ![](wallpaper.jpg){size="cover" opacity="0.5"}
```

| Medium       | Attributes                                    | Classes             |
|--------------|-----------------------------------------------|---------------------|
| Image        | `size`, `position`, `repeat`, `opacity`       |                     |
| Video        | `size`, `opacity`                             | `.loop`, `.muted`   |
| Iframe, PDF  |                                               | `.interactive`      |

`.interactive` lets the clicks reach the iframe. Other media, such as streams, stay
in the heading as they are.

In a handout and on a page, the medium appears below the heading, with its caption,
and without these attributes.

To set a color, put it on the heading: `# Title {color="red"}`. It does not work on
the image, but `data-background-color` on the heading can be combined with an image.
`.inverse` uses the foreground color as background. The attributes of Reveal.js, such
as `data-background-image`, also work on a heading, and Decker copies the file for
them.

### Code blocks

Source code can be a Pandoc code block or a file. A file needs the class `.code`.
Another class names the language for highlighting:

``` markdown
![Some Javascript code](source/code.js){.code .javascript}
```

A fragment selects a part of the file. It is dedented, and the markers themselves
stay out:

``` python
# ---8<--- setup
def setup():
    return 1
# --->8---

# ---8<|--- teardown
def teardown():
    return 2

print("not part of it")
```

- `![](f.py#setup){.code .python}`: everything from `---8<--- setup` up to `--->8---`
  or the next `---8<---`.
- `![](f.py#teardown){.code .python}`: everything from `---8<|--- teardown` up to the
  next empty line.

An unknown name yields an empty block.

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

A PDF is shown by the viewer of the browser, in an `<object>` element:

``` markdown
![The manual](manual.pdf){width="80%" height="500px"}
```

As the slide background (`# ![](manual.pdf)`), it is shown in an iframe.

### 3D polygonal models {#3d-polygonal-models}

3D models are shown in an iframe with a viewer that Decker brings along. The file type
decides on the viewer.

``` markdown
![The Utah teapot](teapot.off){width="600px" height="400px"}

![A robot](robot.glb){width="600px" height="400px" autoplay="1" shadow="0"}
```

For `off`, `obj`, `stl`, `ply`, `pmp`, `xyz` and `agi`, this is
[PMP](http://pmp-library.org). For `gltf` and `glb`, it is
[model-viewer](https://modelviewer.dev/), with these attributes:

`autoplay`
:   Plays the animation of the model. `0` switches it off.

`shadow`
:   The intensity of the shadow, by default `1`.

Viewers need WebGL, so a browser without it shows an empty frame.

### Iframes

``` markdown
![A local page](demo/index.html){w:h="2/1"}

![A web page](https://example.com/){.iframe width="80%"}
```

The other attributes appear on the `<iframe>` as `data-*`, where the embedded page can
read them from `window.frameElement`. In a deck, an iframe has a button that shows it
in fullscreen and is loaded only when its slide comes up. A website can refuse to be
embedded, and then the frame stays empty.

### Videos

``` markdown
![The perfect game](game.mp4){.controls .autoplay start="5" stop="30" width="60%"}
```

| Attribute or class | Effect                                                |
|--------------------|-------------------------------------------------------|
| `.controls`        | Shows the controls of the browser                     |
| `.autoplay`        | Starts when the slide comes up. No effect on a page   |
| `.loop`            | Starts over at the end                                |
| `.muted`           | Without sound                                         |
| `start`, `stop`    | The range to play, in seconds                         |
| `poster`           | An image to show until the video plays. Decker copies the file |
| `preload`          | The attribute of the `<video>` element                |

Without `.controls` or `.autoplay`, nothing starts the video.

### Video streams

The URL scheme names the service, and the path is the id of the video:

``` {.markdown .decker-example file="streams-deck.md"}
# Streams

![From YouTube](youtube:TbppXMGrsvw){.autoplay start="750" width="60%"}

![From Vimeo](vimeo:348290){start="17" width="60%"}

![From Twitch](twitch:851645824){.muted width="60%"}
```

All of them take `.autoplay` and `start`, YouTube and Vimeo also `.loop`, and Vimeo and
Twitch `.muted`. YouTube and Vimeo also know the [YouTube](https://developers.google.com/youtube/player_parameters) or
[Vimeo](https://vimeo.zendesk.com/hc/en-us/articles/360001494447-Using-Player-Parameters)
player parameters. Use a parameter as an attribute (`controls="0"`) or, to set it to
`1`, as a class (`.controls`). Twitch has `start` as a time such as `14m`. It also wants to know the host that embeds the video, and Decker always says
`localhost`, so expect the Twitch player to refuse anywhere else.

With `.loop`, YouTube ignores `start`.

### Audio

``` markdown
![An interview](interview.mp3){.controls start="5" stop="30"}
```

Audio takes `.controls`, `.autoplay`, `.loop`, `.muted`, `start`, `stop` and `preload`.
Without `.controls`, nothing is visible. Only the caption shows, if there is one.

### Graphs and diagrams

A code block with the classes of a language and `.render` runs a program and shows the
resulting SVG image as a figure, so captions and sizes work as for images. A file works
as well, with `.render` alone:

```` {.markdown .decker-example file="graph-deck.md" requires="dot"}
# A graph

``` {.dot .render width="40%"}
digraph {
  node [style = filled]
  A [fillcolor = yellow]
  A -> B
  A -> C
}
```

Caption: Rendered by Graphviz.
````

``` markdown
![The same as a file](graph.dot){.render width="40%"}
```

| Class       | File        | Program                 | Language                              |
|-------------|-------------|-------------------------|---------------------------------------|
| `.dot`      | `.dot`      | `dot`                   | [Graphviz](https://graphviz.org/)     |
| `.gnuplot`  | `.gnuplot`  | `gnuplot`               | [Gnuplot](http://www.gnuplot.info/)   |
| `.plantuml` | `.plantuml` | `plantuml`              | [PlantUML](https://plantuml.com/)     |
| `.mermaid`  | `.mmd`      | `mmdc`                  | [Mermaid](https://github.com/mermaid-js/mermaid-cli) |
| `.d2`       | `.d2`       | `d2`                    | [D2](https://d2lang.com/)             |
| `.tex`      | `.tex`      | `pdflatex` and `pdf2svg`| LaTeX, a complete document, usually with the class `standalone` |

These programs are not part of Decker. `decker check` shows which ones it can find, and
you can change how they are called in [`external-tools`](#external-tools). Mermaid also
needs a browser. If `mmdc` complains that it cannot find Chromium, name one with `-p
puppeteer.json` in the arguments of `external-tools.mermaid.<os>`, with a file that contains
`{"executablePath": "/path/to/chrome"}`.

The image of a diagram has the size that the program gives it, stretched to the width
of its block, so a small graph can get very big and leave the slide. Set a `width` or a
`height`.

`.embed` uses an `<object>` element instead of an `<img>`, so that an SVG can load its
own CSS file. Check the result in all browsers you need.

**A failing program does not fail the build, not even with `--strict`.** Decker prints
`# ERROR:` and generates an SVG that contains the message in red. If the fault is in the
block, correcting it is all you need. If you install a program later on, run `decker
clean`, because the image of the error otherwise stays.

Decker writes the blocks to a directory `.rendered-code` next to the document. Their
names contain a checksum of the content. `decker purge` removes these directories.

### Javascript ES6 modules

A code block with the classes `.javascript` and `.run` runs as a module. The variable
`anchor` is an empty `<div>` where the block is located:

```` {.markdown .decker-example file="module-deck.md"}
# A module

``` {.javascript .run height="100px"}
anchor.innerText = "Hello from Javascript";
```

Caption: Generated in the browser.
````

A `.js` file with the class `.run` also works. Such a module finds its element with
`Decker.anchor(import.meta)`:

``` markdown
![Counting](counter.js){.run width="30%"}
```

``` javascript
const element = Decker.anchor(import.meta);
element.innerText = "Hello from a file";
```

An `import` in a code block is resolved by the browser against the URL of the document,
and Decker does not copy the file. List it in `static-resources`.

### GeoGebra

``` markdown
![The gradient](gradient.ggb){width="800px" height="500px" showToolBar="true"}
```

The applet runs in an iframe with GeoGebra, which Decker brings along. An attribute with
the name of an [applet
parameter](https://wiki.geogebra.org/en/Reference:Math_Apps_Embedding) is passed on,
for example `showToolBar`, `showMenuBar`, `showAlgebraInput`, `enableShiftDragZoom` or
`language`. By default, the applet hides its user interface. `app-width` and
`app-height` set the size of the applet in pixels.

### Charts

A code block with one of the classes `.line-chart`, `.bar-chart`, `.stackedBar-chart`,
`.horizontalBar-chart`, `.radar-chart`, `.doughnut-chart`, `.pie-chart` or
`.polarArea-chart` becomes a chart, drawn by [Chart.js](https://www.chartjs.org/). The
first line holds the labels, and each further line starts with the name of a data set:

```` {.markdown .decker-example file="chart-deck.md"}
# Sales

``` {.bar-chart width="60%" title="Sales per month"}
January, February, March
Apples, 65, 59, 80
Pears, 28, 48, 40
```
````

Charts only work in decks. A handout or a page shows the block as text.

| Attribute or class | Effect                                                |
|--------------------|-------------------------------------------------------|
| `width`, `height`  | The size of the chart                                 |
| `title`            | The title of the chart                                |
| `.fill`, `.nofill` | Fills the area below the lines of a chart, or does not |
| `.log`             | A logarithmic y-axis. It does not work with `.stackedBar-chart` |

An HTML comment with JSON in the block is passed on to Chart.js as `options` and `data`:

``` markdown
January, February
Apples, 65, 59
<!-- { "options": { "plugins": { "legend": { "position": "right" } } } } -->
```

`chart.colors` sets the colors of all charts, and `chart.defaults` merges into the
defaults of Chart.js, see [Appearance](#appearance).

## Slide layout {#slide-layout}

Everything on this page and in the next section applies to decks and handouts.
Pages have no slides: there, these classes and the `. . .` pause have no effect
and the content stays visible.

The body of a slide is a list of *blocks*. A block is a level-2 heading and
everything up to the next level-2 heading. Content in front of the first level-2
heading forms a block without a title, and so does an empty heading (`##`
followed by attributes only).

Attributes on the level-2 heading apply to the block:

| Attribute                                  | Effect                                    |
|--------------------------------------------|-------------------------------------------|
| `.accent0` ... `.accent7`                  | Block in an accent colour                 |
| `align="left"`, `"center"`, `"right"`      | Text alignment, also works on a slide     |
| `width="30%"`                              | CSS width of the block                    |
| `popup="center"`                           | Overlay, see [Popups](#popups)            |

### Area layouts

The class `.columns` or `.grid` on a slide (or `layout="columns"`,
`layout="grid"`) assigns blocks to named areas. A block with the name of an area
as a class starts that area. The blocks after it belong to the same area, up to
the next block with an area class.

``` {.markdown .decker-example file="columns-deck.md"}
---
title: Columns
---

# Two columns and a footer {.columns}

## Left {.left grow="2"}

The wider column.

## Also left

This block is in the left column, too.

## Right {.right}

The narrower column.

## Footer {.bottom}

The full width of the slide.
```

The two layouts have these areas, always in this position:

| Layout     | Row 1                           | Row 2                     | Row 3                                 |
|------------|---------------------------------|---------------------------|---------------------------------------|
| `.columns` | `top` (full width)              | `left`, `center`, `right` | `bottom` (full width)                 |
| `.grid`    | `top-left`, `top`, `top-right`  | `left`, `center`, `right` | `bottom-left`, `bottom`, `bottom-right` |

- Unused areas take no space, the other columns of a row share its width.
- `grow="2"` up to `grow="6"` on the first block of an area sets the relative
  width of its column. It has no effect on a full-width row.
- Using an area twice joins the blocks in document order.
- **Only level-2 headings can start an area.** A `::: left` div or an image with
  the class `.left` is ordinary block content.
- **Content in front of the first area is removed from the slide without a
  warning.** This includes any text directly below the slide heading.

### Grid columns

A div with the class `.columns-N-M...` lays out its child elements in columns
with the given proportions. It can have any number of columns, can be nested and
can be used inside a block. `width` and `height` attributes on a div become CSS
sizes.

``` {.markdown .decker-example file="grid-columns-deck.md"}
---
title: Grid columns
---

# Three columns

Text above the columns.

::: columns-2-3-2
::: {}
First column, with two paragraphs.

The second paragraph.
:::

::: {align="center"}
The wide second column.
:::

::: {width="50%"}
Half of the third column.
:::
:::
```

Each direct child is one cell, so wrap several paragraphs in a div.

### Flowing columns

The class `.split` on a level-2 heading flows that block and the following ones
into two columns. A block with the class `.join` ends them.

``` {.markdown .decker-example file="split-deck.md"}
---
title: Split
---

# Split and join

## First {.split}

Flows into the two columns.

## Second

Also in the two columns.

## Third {.join}

Back to the full width.
```

### Columns from a heading

If the first element on a slide is a level-2 heading that consists of two numbers,
the next two elements become columns with these proportions. A level-3 heading
with three numbers does the same for three elements. Wrap several elements in a
div.

``` {.markdown .decker-example file="number-columns-deck.md"}
---
title: Number columns
---

# One to three

## 1 3

The narrow column.

The wide column.
```

### Popups {#popups}

A block with the attribute `popup` lies on top of the slide with a shadow, usually
together with `.fragment`. The values are `center`, and `center-up` and
`center-down`, which also tilt the block. Handouts show it as a normal block.

``` {.markdown .decker-example file="popup-deck.md"}
---
title: Popup
---

# Popups

## Normal content

Stays below the popup.

## Surprise {.fragment popup="center" width="40%"}

Appears on top after a click.
```

### Fragments

Fragments appear one at a time. There are several ways to create them:

- `.fragment` on a level-2 heading, an image or a span. The effect classes of
  [Reveal.js](https://revealjs.com/fragments/) work as well, for example
  `{.fragment .fade-out}`, and so does `data-fragment-index`.
- A paragraph `. . .` shows everything up to the next one as one fragment. Use it
  only between normal content: a level-2 heading after it ends up inside the
  fragment and does not start a block.
- `.incremental` on a level-2 heading or a div shows the list items one by one.
- `.incremental` on a slide does this for all lists, and also makes every block a
  fragment.
- `.nonincremental` on a block or div exempts its lists. The block itself remains a
  fragment on an incremental slide.

``` {.markdown .decker-example file="fragments-deck.md"}
---
title: Fragments
---

# One after the other

Always visible.

. . .

After the first click.

. . .

[This word]{.fragment .highlight-red} after the second click.

# List items {.incremental}

## Block and items

- One
- Two

## Block only {.nonincremental}

- All at once
- With the block
```

## Notes, handouts and solutions

The classes below select where content appears. They only work on level-1 and
level-2 headings, and apply to the whole slide or block. **On divs, spans and images
they have no effect, and the content appears everywhere.**

| Class       | Deck              | Handout |
|-------------|-------------------|---------|
| `.deck`     | yes               | no      |
| `.handout`  | no                | yes     |
| `.comment`  | no                | no      |
| `.notes`    | as speaker notes  | no      |

A fenced code block with the class `.comment` is removed everywhere, too.

``` {.markdown .decker-example file="selection-deck.md"}
---
title: Selection
---

# Only in the deck {.deck}

Not in the handout.

# Mixed

## For everyone

Everywhere.

## In the handout {.handout}

More detail for reading.

## Note to self {.comment}

Nowhere.
```

### Speaker notes

Speaker notes are visible in the speaker view (key `s`). There are three ways to
write them:

- A block: `## Remember {.notes}`.
- A slide: `# Remember {.notes}` is no slide of its own. It becomes the notes of
  the slide before it.
- A div: `::: notes`. Unlike the other two, **it stays visible in handouts**.

`[text]{.notes}` on a span is not a speaker note, the text stays on the slide.

``` {.markdown .decker-example file="notes-deck.md"}
---
title: Notes
---

# A slide

Visible content.

## Remember {.notes}

Mention the example.

# More notes {.notes}

These belong to the slide above.
```

### Solutions

`.solution` on a slide or block has no effect of its own. It marks content that
`decker publish --lecture` removes from a deck with `lecture.status: upcoming`.

`# Task {.dupsub}` adds a copy of the slide as a vertical sub-slide below it, with
the class `.solution`, for example to work out the solution on the whiteboard. Both
slides have the same id. The copy is not removed by `--lecture`.

### Collapsible content

`.details` on a level-2 heading turns the block into a collapsed element with the
heading as its summary. A div also works, with the summary in an attribute. On pages
it works on headings of any level. `.details .popup` on a heading opens the content
as an overlay.

``` {.markdown .decker-example file="details-deck.md"}
---
title: Details
---

# Collapsed

## Click to read more {.details}

- Hidden at first

::: {.details summary="Also hidden"}
Inside the first one, because it follows its heading.
:::
```

## Quizzes

A quiz is a question on a slide that the audience answers in the browser. Nothing
leaves the browser and nothing is stored. For questions that the whole audience
answers at the same time, see [Live quizzes](#live-quizzes).

Quizzes only work in decks. In the handout `x-handout.html` a quiz is the plain list,
with the correct answers checked. On a page it is also a plain list. Do not put
answers on a page that are supposed to stay secret.

### Question types

Every type has three equivalent classes:

| Type                                     | Classes                                     |
|------------------------------------------|---------------------------------------------|
| [Multiple choice](#multiple-choice)      | `.qmc`, `.quiz-mc`, `.quiz-multiple-choice` |
| [Matching](#matching)                    | `.qmi`, `.quiz-mi`, `.quiz-match-items`     |
| [Insert choices](#insert-choices)        | `.qic`, `.quiz-ic`, `.quiz-insert-choices`  |
| [Free text](#free-text)                  | `.qft`, `.quiz-ft`, `.quiz-free-text`       |

The class goes on a level 2 heading, which can be empty, or on a fenced div:

``` markdown
# Slide title

## The question {.qmc}

Some text that belongs to the question.

- [ ] Wrong
  - Shown as a tooltip for this answer.
- [X] Correct
  - Shown as a tooltip for this answer.

# Another slide

::: qmc
- [ ] Wrong
- [X] Correct
:::
```

A question must be on a slide, so it needs a level 1 heading above it. Answers are a
task list. A nested list is the tooltip of an answer. Each of its items is one line of
the tooltip.

### Multiple choice

Each answer is a box. Clicking it shows whether the answer is right or wrong, along
with its tooltip. Several answers can be correct.

### Matching

The audience sorts items into buckets by drag and drop. *Show Solution* marks the
result, and complains if no item was moved yet. The question uses a [definition
list](https://pandoc.org/MANUAL.html#definition-lists): the terms are the buckets, the
definitions are their items. An exclamation mark makes a distractor, which belongs in
no bucket, or an empty bucket:

``` markdown
## Sort these {.qmi}

Question text

Bucket A
: Item 1
: Item 2

Bucket B
: Item 3

!
: A distractor

Empty bucket
: !
```

With `.plain`, the items are a lettered list, and each bucket gets a selection box
instead.

### Insert choices

A text with selection boxes in it. Each task list becomes one selection box at its
place in the text, and the choice is judged at once. A list with one item is still a
selection box, with that one option. Paragraphs of a question are joined into running
text.

``` markdown
## {.qic}

- [X] Haskell
  - Because of Pandoc.
- [ ] Java

is what Decker is written in. It builds upon

- [X] Pandoc
- [ ] PowerPoint

.
```

### Free text

An input field. The answer is judged on `Enter`, ignoring case and surrounding space.
In a plain list **every** item is a correct answer. To name wrong answers, for example
because they deserve a comment, use a task list:

``` markdown
## {.qft}

What is the fourth letter of the alphabet?

- [ ] C
  - One too early.
- [X] D
  - Right.
```

Any answer that is not listed counts as wrong. *Show Solution* lists the answers,
*Reset* clears the field.

### Style

The default style is `fancy`. `plain` is more compact, and also has the variant
`.inline` for multiple choice, which puts the answers in a row. The classes `.plain`
and `.fancy` choose it for one question, `quiz.style` for a whole document or project:

``` yaml
quiz:
  style: plain
```

``` markdown
## {.qmc .plain .inline}

- [X] 4
- [ ] 5
```

The labels of the buttons and fields come from the [dictionary](#dictionary), in the
language of `lang`.

### Options of one question

A YAML code block in a question sets its options. The block is not shown. `lang`
chooses the language of its labels, `quiz.style` its style.

```` markdown
## {.qft}

``` yaml
lang: de
quiz:
  style: plain
```

Which letter comes first?

- A
````

So a question cannot show a YAML code block as part of its text. The fields `score`,
`category`, `lectureId` and `topic` are accepted, but nothing uses them.

## Live quizzes

In a live quiz the whole audience answers on their phones and the results appear on
the slide. It needs a quizzer server, which is not part of Decker, and the `quizzer`
plugin:

``` yaml
decker-plugins:
  quizzer: true
quizzer:
  url: https://quiz.example.com/
```

The `tudo` and `mono` packs load the plugin. `tudo` also sets the server, with `mono` the
project has to.

A slide can have one live quiz. A second one replaces the slide with an error message.
A quiz is a div with one of these classes:

| Type      | Classes                                                                   |
|-----------|---------------------------------------------------------------------------|
| Choice    | `.quizzer`, `.quizzer.choice`, `.quizzer-choice`, `.quizzer-mc`, `.quizzer-multiple-choice` |
| Selection | `.quizzer.selection`, `.quizzer-selection`, `.quizzer-ic`, `.quizzer-item-choice` |
| Free text | `.quizzer.freetext`, `.quizzer-freetext`, `.quizzer-free-text`, `.quizzer-ft` |
| Matching  | `.quizzer.assignment`, `.quizzer-assignment`, `.quizzer-match-items`, `.quizzer-mi` |

``` markdown
# Choice

::: {.quizzer .choice}
Which of these is not arithmetic?

- [ ] `x + y`
  - The reason, shown with the result.
- [x] `x ^ y`
:::

# A survey

::: quizzer-mc
Which semester are you in?

- [ ] 1 to 2
- [ ] 3 to 4
:::
```

- **Choice.** With more than one correct answer, the audience can pick several. Without
  a correct answer it is a survey.
- **Selection and free text.** `[#1]`, `[#2]` and so on in the text mark where the fields
  go, and a list provides the answers of each. Fields without a mark are added at the
  end. A horizontal rule separates two lists. `placeholder` on the div sets the hint in
  the input field of free text.
- **Matching.** A definition list as for [matching](#matching), with `!` for distractors.

``` markdown
::: quizzer-ic
Decker is written in [#1] and uses [#2].

- [x] Haskell
- [ ] Java

---

- [x] Pandoc
- [ ] Word
:::

::: {.quizzer-ft placeholder="Your answer"}
The result is [#1].

- [x] 4
- [x] four
:::
```

Without presenter mode, a live quiz works like a normal quiz for whoever opens the
deck. In [presenter mode](#presenter-mode), a slide with a quiz shows a poll button at
the bottom, and a dot for the connection to the server. The first click starts the poll
and shows a QR code with the session id. The audience joins with it. A counter shows
how many have answered. The second click ends the poll and shows the result: a chart,
a word cloud for free text, a flow diagram for matching. Changing the slide also ends
the poll.

`quizzer.audio` plays sounds during a poll. `start`, `loop` and `end` each take a URL or
`default` for the built-in sound, `volume` is a number up to `1.0`.

### Polls on exam questions

An [exam question](#exam-questions) in a deck can be used as a poll. This is independent
of the `quizzer` plugin and needs its own server in `poll-server`. `mono` sets it.

``` markdown
---
poll-server: wss://polls.example.com/quiz
---

# A question

![](recursion-quest.yaml){.question .poll}
```

In presenter mode the question gets the buttons *QRCode*, *Poll* and *Stop*. Only
`MultipleChoice` questions can be polled. With `save-polls: true` and a running Decker
server, the votes of the deck `x-deck.md` are stored in `x-<session>-poll.json` next to
it.

## Live code

Code blocks can be edited and executed right on the slide. Execution is done by
a [codapi](https://codapi.org) server that runs the snippets in sandboxed Docker
containers. Live code needs fragment templates to be enabled:

``` yaml
experiments:
  fragment-templates: true
```

The server is declared once per deck, either with a link macro anywhere in the
deck or with the meta data variable `codapi.server`:

``` markdown
[§live-code-server](https://example.com/codapi/v1)
```

A code block with the class `.live-code-` (the trailing dash is intentional, it
marks a fragment template) becomes an editor with a *Run* button:

```` markdown
``` {.live-code- language="Python"}
msg = "Hello, World!"
print(msg)
```
````

The following attributes are supported:

`language`
:   The language used for syntax highlighting and, if `sandbox` is not given,
    the codapi sandbox.

`sandbox`
:   The codapi sandbox that runs the code, for example `ghci`.

`height`
:   The maximum height of the code box, for example `height="200px"`.

`caption`
:   A figure caption below the code box.

`depends-on`
:   The id of another live code block whose code is run before this one. Add
    `.display-none` to a block that should only provide code for others.

`template`
:   A reference (`#` plus id) to a `.live-code-prelude-` block that wraps the
    code. The prelude contains `##CODE##` where the code is inserted.

`result`
:   Where the result of a run is shown. See below.

`result-height`
:   The height of the result area for `result="reserve"`, default `6em`.

### Result placement

By default, the result of a run is placed so that the layout of the slide does
not change. The `result` attribute selects the placement:

`auto` (default)
:   `split` for code boxes with at least six lines, `flip` for smaller ones.

`split`
:   The code box keeps its height. Code and result share it, the result takes
    at most half of it. The code scrolls.

`flip`
:   The result replaces the code in the same box. ✕ flips back to the code,
    *Output* shows the last result again without running the code.

`overlay`
:   The result covers the lower half of the code box. It fades out while the
    code is edited.

`float`
:   The result floats over the content below the code box. If there is not
    enough room on the slide, it opens upwards.

`reserve`
:   The result area is always shown with a fixed height (`result-height`), even
    before the code is run.

`below`
:   The result is added below the code box. The slide grows, which may push
    content off the slide.

```` markdown
``` {.live-code- language="Python" result="reserve" result-height="4em"}
print(42)
```
````

### Running your own server

The codapi server must answer cross-origin requests from wherever the decks are
served, including `http://localhost:<port>` during development. Browsers send
the port as part of the `Origin` header, so the server can simply echo it. For a
server behind Caddy:

``` caddy
(cors) {
    @options method OPTIONS

    header {
        Access-Control-Allow-Origin {header.Origin}
        Access-Control-Allow-Methods "GET, POST, OPTIONS"
        Access-Control-Allow-Headers "Content-Type, Authorization"
        Access-Control-Max-Age 3600
        Vary Origin
        defer
    }

    respond @options 204
}
```

Import the snippet in the block that proxies to codapi, before the
`reverse_proxy` directive.

## Sage

The packs [`mario`](#mario) and [`tudo`](#tudo) load a plugin that makes code blocks
editable and runs them on the public [SageMathCell](https://sagecell.sagemath.org)
server. So the browser needs a connection to that server during the presentation.
A div with the class `sageCell` holds the code. The class of the code block selects
the language: `sage`, `python`, `rr` (for R) or `octave`.

```` markdown {.decker-example file="a-deck.md" project="sage"}
---
title: Sage cells
---

# Evaluate this

::: {.sageCell width="100%" height="400px"}
``` sage
print(6*7)
```
:::
````

The pack is selected in `decker.yaml`:

``` yaml {.decker-example file="decker.yaml" project="sage"}
resource-pack: exe:mario
```

The cell has an *Evaluate* button and a menu to change the language. `width` and
`height` set the size of the cell, the defaults are `100%` and `500px`. With other
packs, on pages and in handouts, the block stays a plain code block.

# During a presentation

Everything in this chapter happens in the browser, in a running deck. The plugins are
part of Decker, and most of them need no configuration.

Some features write files into the project. These only work if the deck is served by
Decker, either with `decker --server` or with `decker serve`. The files end up next to
the source of the deck, and the next build copies them to `public/`, so that they can
be published. Only the file types that Decker expects can be uploaded.

## Presenter mode

Press `P` three times in quick succession, or use the menu, to switch presenter mode on
and off. It hides nothing from the slides, but shows the tools for the person in front
of the room. The [whiteboard](#whiteboard), [live quizzes](#live-quizzes) and [live
captions](#live-captions) only appear in it. It also keeps the display awake and stops
the page from reloading on a rebuild. The URL parameter `?presenter` starts a deck in
presenter mode.

The link `presenter` in [`index.links`](#index-pages) of an index page opens a deck this
way.

## Menu

The button with the three bars at the top left opens the menu. It lists all slides, with
the current one marked, and has these buttons:

- back to the index page
- [search](#search)
- color mode (disabled if `colorscheme` is set to `light` or `dark`)
- fullscreen
- printing to PDF, see [`decker pdf`](#decker-pdf)
- [handout mode](#notes-handouts-and-solutions) (`H` three times)
- [presenter mode](#presenter-mode)
- [recording](#recording)
- [accessibility](#accessibility)

The menu has no options. The plugin does not read a key `menu`.

## Search

`Ctrl`+`F` or `Cmd`+`F` opens the search in a deck, as does the button in the menu.
`Enter` goes from one match to the next, also across slides. `Esc` closes it. In handout
mode the keys open the search of the browser instead.

This only searches the current deck. To search across all decks, see [`decker
search-index`](#decker-search-index).

## Whiteboard

The whiteboard draws on slides and on extra pages below them. It only works in
[presenter mode](#presenter-mode).

`W`, or the button at the bottom left, switches drawing on and off. While it is on, that
corner shows the current tool. A click on it opens the panel with all of them:

| Button          | Key                 | Action                                        |
|-----------------|---------------------|-----------------------------------------------|
| Pen             |                     | Draw. Pen settings has the colors and widths. |
| Eraser          | `E`                 | Erase strokes. `E` switches back to the pen.  |
| Laser pointer   | `L`                 | Strokes that fade. `L` switches back.         |
| Undo            | `Ctrl`/`Cmd`+`Z`    | Undo, up to 10 steps                          |
| Add page        |                     | Add an empty page below the slide             |
| Grid            |                     | Show a grid on the extra pages                |
| Auto-save       |                     | Switch automatic saving on and off            |
| Save            |                     | Save now                                      |
|                 | `Delete`            | Delete everything on the current slide        |
|                 | `1` to `7`          | Pen color                                     |
|                 | `8`, `9`, `0`       | Pen width                                     |

With a mouse, the right button is the laser pointer and the middle button the eraser
while the pen is selected. With a stylus, its button is the laser pointer and its eraser
tip the eraser.

### Saving

Saving does two things. It puts `x-annot.json` of the deck `x-deck.md` into the
downloads folder of the browser, and it uploads the same file next to the source of the
deck. A deck loads this file when it starts. The next build copies it to `public/`, and
[`decker pdf`](#decker-pdf) prints the annotations too. To see saved annotations with
`decker serve`, build first, because it serves `public/` as it is.

With auto-save, which is the default, this happens when you leave the deck, not on every
change. Without it, the browser warns about unsaved annotations when you leave. Without
a Decker server only the download works, and the file has to be copied next to the
source by hand.

### Options

``` yaml
whiteboard:
  colors: ["var(--base07)", "red", "#00ff00"]  # the colors of the pen settings
  autosave: false       # default is true
  autotoggle: true      # switch on when a stylus hovers, default is false
```

The keys `1` to `7` expect the 16 default colors, so they do not fit a list of your own.
For extra pages in a PDF, set `reveal.pdfMaxPagesPerSlide` to a sufficiently large
value.

## Audience Feedback

The audience of a deck can ask questions about each slide, and vote on the questions of
others. The questions are stored on a server and are visible to everybody who opens the
deck. The author can answer them there, or change the deck.

The plugin needs a server, the Decker Engine, which is not part of Decker. Without a URL
nothing appears:

``` yaml
feedback:
  base-url: https://engine.example.com/feedback/
```

The deck then shows a button that opens the list of questions of the current slide.
Questions are written in Markdown and sent with `Shift`+`Enter`. The `decker-plugins`
key `feedback` switches the plugin off altogether.

### Endpoints with authorization

There are two modes of operation, depending on how the server is deployed.

If the server runs behind a proxy with Basic Authentication, questions can only be added
by a user that the proxy has authenticated. Administrators are recognized automatically,
no further authorization is necessary.

### Public endpoints

If the server is publicly available, a user is assigned a token. It allows them to later
edit or delete all questions that were added with that token. The token can be entered
by hand or stored in the local storage of the browser. Administrators log in with a
username and a password.

### Administrators

Administrators can answer, edit or delete all questions of a deck, and mark them as
answered.

### Deck Identification

Decks are identified by their public URL. This is a problem if a deck is served
locally, for example from `http://localhost:8888/lecture/intro-deck.html` during a
recording, but is supposed to show the questions of the published version. For this
situation, set an id that stays the same:

``` yaml
feedback:
  deck-id: https://example.com/lecture/intro-deck.html
```

Any text will do. The questions shown locally are then the ones added to the published
deck. The id also connects a deck to [`no-index`](#documents-and-title-slide).

## Accessibility

Accessibility mode adapts a deck for screen readers and keyboard use. It is switched by
pressing `A` three times in quick succession, by the button in the menu, or with the URL
parameter `?a11y`, which is what the link `a11y` on an [index page](#index-pages) does.
The parameter is also added to the URL while the mode is on, so a bookmark keeps it.

In this mode:

- Videos and audio get controls and stop playing by themselves.
- Formulas get MathJax's menu and semantic enrichment. With the URL parameter, also its
  speech and Braille output.
- [Recordings](#recording) start with their captions switched on.

These apply to every deck, in any mode:

- Screen readers hear the number of the slide before its title.
- Slides that are not visible can not be reached with the tab key.
- Images get their label from the [title of the image](#figures-and-captions), or else
  from its caption, so write one of them.

## Recording

A deck can record the screen, a microphone and a camera while you present, and play the
result back on the slides.

### Making a recording

1. Serve the deck with `decker --server` or `decker serve`.
2. Press `R`, or use the menu. The browser asks what to share and for permission to use
   the devices. A panel at the side has the selection of microphone and camera and the
   recording level. `V` shows the camera.
3. Press `R` to start. If there is a recording already, you choose between adding to it
   and replacing it.
4. Press `R` to pause and resume.
5. Press `Z` three times in quick succession to stop.

The result is `x-recording.webm` and `x-times.json` for the deck `x-deck.md`, both next
to its source and in the downloads folder of the browser. The times say which slide was
shown when.

A recording in the browser's format is not what gets played. Turn it into
`x-recording.mp4` with [`decker crunch`](#decker-crunch), or at once with the option
[`-P`](#p---poser) of the server. [`decker transcribe`](#decker-transcribe) then
creates the captions. The next build copies all of it to `public/`.

### Playback

A deck with `x-recording.mp4` and `x-times.json` gets a play button on the slides that
are part of the recording. Playback starts at the current slide. Captions from
`x-recording-en.vtt` and `x-recording-<lang>.vtt` are offered in the player, where
`<lang>` is `whisper.lang`.

### Options

``` yaml
explain:
  video: https://example.com/intro-recording.mp4 # instead of x-recording.mp4
  times: https://example.com/intro-times.json    # instead of x-times.json
  recWidth: 1920      # size requested for the screen
  recHeight: 1080
  camWidth: 1280      # size requested for the camera
  camHeight: 720
```

The camera image can have a green screen removed:

``` yaml
explain:
  useGreenScreen: true
  greenScreenBackground: images/background.jpg
  greenScreenKey: { r: 0, g: 255, b: 0 }
  greenScreenSimilarity: 0.4
  greenScreenSmoothness: 0.08
  greenScreenSpill: 0.1
```

## Live captions

Live captions turn your speech into text during a presentation, with the speech
recognition of the browser, in a window of their own. This needs a browser with the Web
Speech API, such as Chrome. Depending on the browser, the speech goes to a service of its
vendor, and Decker asks you to agree to that first.

``` yaml
live-captions: true
speech-recognition-language: en-US # default is the language of the browser
caption-server: https://captions.example.com
```

In [presenter mode](#presenter-mode) a button appears at the top right. It opens the
window with the text, which you can move to the projector. Closing the window does not
stop the captions, the button does.

`caption-server` is optional, and such a server is not part of Decker. With one, Decker
shows a QR code, and the audience can read the captions on their own devices. The `tudo`
pack sets all three keys.

# Chatty

Chatty is a chat assistant for decks. Its answers come from OpenAI, and it can
search the course material in an OpenAI vector store. The browser talks to the
`decker-chatty` proxy, which holds the OpenAI API key. Enable it with
`decker-plugins`, see [Plugins and page features](#plugins-and-page-features).

## Vector store

`decker chatty` builds annotated Markdown from all decks and pages that are not
drafts, writes it to the directory `chatty/` and syncs it to the vector store.
Without a `chatty.server` it stops after writing the Markdown, with exit
status 1.

Decker does not need an OpenAI API key for this. All vector store requests go
through the decker-chatty proxy at `chatty.server`. Each request is signed with
the deck-config key for `chatty.prompt` from `chatty-key.json`, the same key used
to seal the chatty configuration. The proxy checks the signature and forwards the
request to OpenAI with the API key it keeps for that prompt.

If no `chatty.vector-store-id` is configured, or the configured store no longer
exists, a new vector store is created and its id is printed for you to paste into
`decker.yaml`. `chatty.vector-store-name` sets the name of a new store (defaults to
`decker`).

With the `--prune-files` option, after the sync every assistant-purpose file
object in the OpenAI project that is *not* attached to the store is deleted. This
removes files left dangling in the project's file storage, for instance after a
vector store was deleted in the OpenAI dashboard. This is destructive and
operates on all assistant-purpose files in the project, not only those uploaded
by decker.

If a vector store is configured (meta variable `chatty.vector-store-id:`),
`decker publish -l` also generates the annotated Markdown for every published
deck and page and syncs it to the store. The exact same filtering is applied: drafts
never reach the store, and for `upcoming` lectures the `.solution` content is
stripped before upload. Plain `decker publish` does not touch the store.

To keep a document published while excluding its generated Markdown from
Chatty, set the following in its YAML front matter:

``` yaml
chatty:
  exclude: true
```

This applies to both `decker chatty` and `decker publish -l`, including
`index.md`.
The default is `false`. It does not exclude content included by another
document, or files uploaded directly through `chatty.extra`.

Additionally, the meta variable `chatty.extra:` may list directories whose files
are uploaded to the same vector store as is, without any processing:

``` yaml
chatty:
  vector-store-id: vs_...
  extra:
    - handouts
    - resources/glossary
```

Directories are searched recursively, relative to the project root. This is also
the way to make source code, such as Java examples, available to Chatty. Two
kinds of files are skipped:

-   Dotfiles and dot directories (`.DS_Store`, `.git/`, ...).
-   Files whose type OpenAI file search does not accept. Only these extensions
    are uploaded: `.c`, `.cpp`, `.cs`, `.css`, `.doc`, `.docx`, `.go`,
    `.html`, `.java`, `.js`, `.json`, `.md`, `.pdf`, `.php`, `.pptx`, `.py`,
    `.rb`, `.sh`, `.tex`, `.ts`, `.txt`. Skipped files are summarized per
    directory in the log, for example
    `# chatty.extra: skipping 42 file(s) with unsupported extensions in code: .class .xml`.

The store is reconciled on every run, so changed files are re-uploaded and files
removed locally (from `chatty/` or from the `chatty.extra` directories) are
removed from the store. Reconciliation keeps exactly one current version per
file: stale duplicates and files lacking decker's bookkeeping attributes are
removed. If the configured store no longer exists (for example because it was
deleted in the OpenAI dashboard), `decker publish -l` skips the sync with a warning
instead of failing; run `decker chatty` to create a fresh store.

Note that deleting a vector store in the OpenAI dashboard does *not* delete the
underlying uploaded file objects, which then linger in the project's file
storage. Use `decker chatty --prune-files` to clean them up (see below).

## System prompt and sealed config

The chat assistant's system prompt, model and model parameters are authored in
meta and **sealed into the published deck** at compile time. This replaces
OpenAI's deprecated stored prompts. The work is split so each side changes at its
own pace:

- **Deck author (changes often):** the system prompt, model and params, written
  in markdown and re-sealed on every `decker` build.
- **Sysadmin (changes rarely):** the OpenAI API key and the encryption key, held
  only in the `decker-chatty` proxy.

How it flows at runtime:

1. Decker encrypts the author config into `chatty.sealed-config` and strips the
   plaintext, so the system prompt never appears in `public/`.
2. The browser forwards the opaque blob to the proxy on every chat request and
   does no crypto itself.
3. The proxy decrypts the blob, then calls OpenAI with the sealed model,
   instructions and `file_search` tool — ignoring any model/instructions/tools a
   client tries to supply.

The blob is AES-256-GCM with the `chatty.prompt` value bound as authenticated
data, so a client can neither read the prompt nor move a blob between decks. (It
does **not** protect against prompt-injection extraction once the chat is live.)

### Author configuration (meta)

Set these under `chatty:` in `decker.yaml` (project-wide) or in a deck's YAML
frontmatter:

``` yaml
chatty:
  prompt: pmpt_tutor                 # selector into the proxy config (see below)
  server: "https://example.org/chatty"
  greeting: |
    I'm **Prof. Bot**, your AI tutor for this course.
  identity:
    name: Prof. Bot
    launcher:
      icon: { type: fontawesome, value: fa-robot }
    bot:
      icon: { type: emoji, value: "🤖" }
    user:
      icon: { type: emoji, value: "🤔" }
  instructions: ./prompts/tutor.md   # path to a file, or inline prompt text
  model: gpt-4.1
  params:
    temperature: 0.2
  vector-store-id: vs_abc123         # the file_search store, sealed into the blob
```

| Key                | Required | Meaning |
|--------------------|----------|---------|
| `prompt`           | yes      | A name that selects an entry in the proxy's `config.json`. **No longer an OpenAI stored-prompt id** — pick any stable string, e.g. `pmpt_tutor`. Also used as the encryption AAD. |
| `server`           | yes      | URL of the `decker-chatty` proxy endpoint the deck POSTs to. |
| `greeting`         | no       | Markdown greeting shown as the first Chatty message. |
| `identity`         | no       | Name, launcher tooltip and launcher/bot/user icons. Icons use `{ type, value }` with `fontawesome`, `image`, `text`, or `emoji`. |
| `instructions`     | yes      | The system prompt. If the value names an existing file (e.g. `./prompts/tutor.md`) its contents are used; otherwise the value is taken as inline text. |
| `model`            | no       | OpenAI model id. Defaults to `gpt-4.1`. |
| `params`           | no       | Extra Responses-API parameters merged into the request (e.g. `temperature`, `max_output_tokens`). Defaults to none. |
| `vector-store-id`  | no       | The vector store used for `file_search` (the same id `decker chatty` uploads to). Omit to run without retrieval. |

`instructions`, `model`, `params` and `vector-store-id` are sealed and then
**removed** from the published meta. `prompt` and `server` remain (the client
needs them); `chatty.sealed-config` is added.

### The encryption key (`chatty-key.json`)

The key lives in a git-controlled **`chatty-key.json`** at the project root. It is
read directly by the sealing code — never via `decker.yaml`, and never copied to
`public/`. The simplest form is a single 32-byte key written straight to the file:

``` bash
openssl rand -base64 32 > chatty-key.json
```

That bare base64 line is accepted as-is. Equivalently you may quote it as a JSON
string:

``` json
"Yk3v...base64-32-bytes...=="
```

Or, when one project serves several prompts, use a per-prompt JSON map:

``` json
{
  "pmpt_tutor": "Yk3v...==",
  "pmpt_grader": "Qp9a...=="
}
```

The key must decode to exactly 32 bytes. If `chatty-key.json` is absent — or
present but unparseable — sealing is skipped and the deck falls back to the
legacy path; the build log says which, and the plaintext chatty config is still
stripped from the output so the prompt never leaks.

Decker logs, per chatty deck, whether it was **SEALED** (with a model /
instructions / vector-store summary) or left in **LEGACY** mode (with the reason).
Watch for the `# chatty:` lines during a build to confirm sealing actually ran.

### Proxy configuration (`decker-chatty`)

The proxy's `config.json` maps each `prompt` id to its two secrets:

``` json
{
  "port": 3000,
  "prompts": {
    "pmpt_tutor": {
      "apiKey": "sk-...",
      "deckConfigKey": "Yk3v...=="
    }
  }
}
```

- `apiKey` — the OpenAI API key used for that prompt's requests.
- `deckConfigKey` — **must be byte-for-byte the same** base64 key the author put
  in `chatty-key.json` for that prompt id. Keys are validated (32 bytes) at
  startup, so a mismatch in length fails fast; a content mismatch surfaces as a
  `400 invalid sealed config` per request.

Run it (typically behind a reverse proxy that terminates TLS and handles client
authentication):

``` bash
cd decker-chatty
npm install
npm start          # or install the systemd service, see its README
npm test           # optional: crypto-contract + body-assembly unit tests
```

### End-to-end setup checklist

1. **Generate a key:** `openssl rand -base64 32`.
2. **Author side:** put the key in `chatty-key.json` at the project root; add the
   `chatty:` block (above) to `decker.yaml`; keep the repo private to authors.
3. **Proxy side:** add a `prompts.<prompt>` entry to the proxy `config.json` with
   the OpenAI `apiKey` and the same key as `deckConfigKey`; start the proxy and
   expose `server` over HTTPS.
4. **Vector store (optional):** run `decker chatty` to create/sync the store and
   set `chatty.vector-store-id`.
5. **Build & publish:** `decker publish`. Verify the published
   `public/<hash>.json` contains `chatty.sealed-config` and **not** the prompt
   text, then open a deck and start a chat.

### Rotation and security notes

- Rotating the **OpenAI `apiKey`** is free: edit the proxy `config.json`, no deck
  recompile needed (the payoff of keeping the two secrets separate).
- Rotating the **`deckConfigKey`** requires updating both `chatty-key.json` and
  the proxy, then **recompiling** every deck that uses that prompt id.
- `chatty-key.json` grants no more than repo read access already does — it does
  **not** expose the OpenAI API key, which lives only in the proxy. Still, keep
  any repo carrying it restricted to authors; if it is ever made public, rotate
  the key and recompile.
- Old decks published before this change (no sealed blob) keep working through a
  legacy pass-through in the proxy, until OpenAI removes stored prompts.

# Exam questions

Each exam question is a YAML file `*-quest.yaml`. Decker renders a preview
`private/<path>-quest.html` for each question and the catalog
`public/exam-builder/questions.json` that the [exam
builder](#decker-exam-builder) uses.

``` yaml
LectureId: functional
TopicId: recursion
Title: Base case
Points: 1
Difficulty: Easy            # Easy | Medium | Hard
Comment: ""
Question: |
  What does `take 3 (iterate (*2) 1)` return?
Answer:
  tag: MultipleChoice
  Choices:
    - Correct: true
      TheAnswer: "`[1,2,4]`"
    - Correct: false
      TheAnswer: "`[2,4,8]`"
```

`LectureId`, `TopicId`, `Title`, `Points`, `Question`, `Answer`, `Difficulty`
and `Comment` are required. `Exam` (default `true`; `false` for polls and
surveys) and `ShuffleAnswers` (default `true`) are optional. `Question` and
the answer texts are Markdown. Unknown fields are reported as
`quest-unknown-field`.

The answer types are selected by `tag`:

| `tag`             | Fields                                                   |
|-------------------|----------------------------------------------------------|
| `MultipleChoice`  | `Choices: [{TheAnswer, Correct}]` (`Correct`: boolean)   |
| `MultipleAnswers` | `WidthInMm`, `Answers: [{Detail, Correct}]` (both text)  |
| `FillText`        | `FillText`, `CorrectWords` (list of text)                |
| `FreeForm`        | `HeightInMm`, `CorrectAnswer`                            |
| `Numerical`       | `CorrectNumber` (integer)                                |

Questions that share `LectureId` and `TopicId` are variants of each other; a
generated exam draws one of them per student.

# Hacking on Decker

This chapter is for people who work on Decker itself. The
[readme](https://github.com/decker-edu/decker/blob/master/readme.md) explains how to
install it from source and how to use the dev container.

## Building and testing

``` sh
stack build -j8                             # Build
stack test -j1                              # Run the tests
stack run -- decker --project-dir=DIR html  # Run the development build on a project
make install                                # Clean build, install to ~/.local/bin
make upgrade-third-party                    # Copy vendor resources after a submodule update
```

A development run serves support files directly from `resource/`. An installed Decker
copies them, so check changes to resource packs with an installed Decker as well.

## Conventions

- **Formatting.** Haskell code is formatted with
  [ormolu](https://github.com/tweag/ormolu), through the Haskell Language Server.
- **Language.** The language is `GHC2021` with `OverloadedStrings`, `QuasiQuotes` and
  `MultiWayIf`. Many modules use `NoImplicitPrelude` with `Relude`.
- **Package.** `decker.cabal` is generated by hpack and not checked in. Change
  `package.yaml` instead.
- **Versions.** Each feature increments the version in `package.yaml`. Breaking
  changes increment the second number, fixes the third.
- **Configuration.** Never pass configuration through environment variables. Command
  line options become meta values or flags.
- **Resources.** Templates and support files live in `resource/`, and each directory
  there is a [pack](#bundled-packs). Third-party code comes from the submodules in
  `third-party/`.

## Documentation

`stack test` keeps this guide and the agent topics in
`resource/decker/template/agent-guide/` in step with the code:

- Every command, target and option needs a heading in this guide.
- Every key in the `default.yaml` of the base pack needs an explanation in this guide.
- No section may be empty, and links within the guide must lead to a heading.
- Code blocks with the class `decker-example` are built, in both the guide and the
  topics. The tests in `test/AgentGuideTests.hs` describe their attributes.
- A new diagnostic code needs a row in the table of the `index` topic and a fixture
  in `test/fixtures/diagnostics/`.

The guide is built with `make -C users-guide`, which needs Pandoc. A push to `master`
publishes it through the workflow `.github/workflows/jekyll-gh-pages.yml`. The anchors
of the links are therefore Pandoc's, and some differ from those on GitHub's rendering
of the file: `#decker-publish` is `#-decker-publish` there.
