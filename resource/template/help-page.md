---
title: Decker Help Page
---

# decker

A markdown based tool for slide deck creation.

## Description

*Decker* behaves very much like a build tool. It works recursively on the
current directory and all subdirectories. Markdown files ending on `.md` in
those directories are processed and converted to either a reveal.js slide show
or a single page HTML document, depending on the file name.

`*-deck.md`

:   Files with this ending are processed as *silde decks*. From one source file
    potentially four different targets can be generated:

    -   `*-deck.html`: A reveal.js based slide show
    -   `*-handout.hmtl`: A single HTML document containing the structured slide
        show content along with the speaker notes
    -   `*-deck.pdf`: A PDF version of the slide show

`*-page.md`

:   Markdown files ending on `*-page.md` are translated into corresponding
    single page HTML or PDF documents.

*Decker* understands the notion of a *project directory*. The first directory
above the current directory that contains a `.git/` directory is assumed to be
the project directory. If `.git/` cannot be found, the current directory is used
as the project directory. All path calculations are performed either file
relative or relative to the project directory.

The resulting HTML documents and all necessary resources are placed in the
`public` directory within the project directory. The content of `public` is
self-contained and can directly be served by any static HTML server. No external
sites will be referenced during rendering of the slide decks (except for
explicit external HTML references like video, image or iframe links, of course).

## Usage

```
decker [options] [target]
```

### Options

*Decker* supports most common `make` options:

    -B, --always-make           Unconditionally make all targets.
    -d[=FILE], --debug[=FILE]   Print lots of debugging information.
    -h, --help                  Print this message and exit.
    -j[=N], --jobs[=N]          Allow N jobs/threads at once [default CPUs].
    -k, --keep-going            Keep going when some targets can't be made.
    -l, --lint                  Perform limited validation after the run.
    --live[=FILE]               List the files that are live [to live.txt].
    --numeric-version           Print just the version number and exit.
    -o FILE, --old-file=FILE, --assume-old=FILE
                                Consider FILE to be very old and don't remake it.
    --old-all                   Don't remake any files.
    -r[=FILE], --report[=FILE], --profile[=FILE]
                                Write out profiling information [to report.html].
    -s, --silent                Don't print anything.
    --sleep                     Sleep for a second before building.
    -q, --quiet                 Don't print much.
    --no-time                   Don't print build time.
    --touch                     Assume targets are clean.
    -V, --verbose, --trace      Print tracing information.
    -v, --version               Print the version number and exit.
    -w, --print-directory       Print the current directory.
    -W FILE, --what-if=FILE, --new-file=FILE, --assume-new=FILE
                                Consider FILE to be infinitely new.

### Targets

`decker help`

:   Prints this document to stdout in Markdown syntax. Format to your liking
    using `pandoc`:

    ``` {.sh}
    decker help | pandoc -s -t html > decker-help.html
    ```

`decker check`

:   Checks the availability of the external dependencies. Decker still works
    without these, but some functionality will be unavailable.

`decker html`

:   Builds HTML versions of all available documents. This is the default.

`decker pdf-decks`

:   Builds PDF versions of all slide decks.

`decker watch`

:   Builds HTML versions of all documents and then watches for document changes.
    Each change to a watched document triggers a rebuild. Watching can be
    terminated with `^C`.

`decker server`

:   Like `decker watch`. Additionally, a local web server is started that serves
    the generated HTML files. The `index.html` document is automatically openend
    in the browser. Changed files are reloaded in the browser. The server can be
    terminated with `^C`.

`decker example`

:   Writes a few example files to the current directory. To start exploring
    decker type

    ``` {.bash}
    > mkdir first-contact
    > cd first-contact
    > decker example
    > decker server
    ```

    and make some changes to the Markdown files. `example-deck.md` contains the
    source code for a slide deck that explains some of deckers features.

`decker clean`

:   Recursively removes all generated files from the project directory.

`decker plan`

:   Prints a list of all source files found in the project directory and the
    targets calculated from them.

`decker publish`

:   Publishes the generated files to a remote location using `rsync` if the
    location is specified in the meta data. The keys `rsync-destination.host`
    and `rsync-destination.path` specify the publishing destination.
