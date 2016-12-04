# decker

A markdown based tool for slide deck creation.

## Usage

*decker* behaves very much like a build tool. It works recursively on the current
directory and all subdirectories. Markdown files ending on `.md` in those
directories are processed and converted to either a reveal.js slide show, a HTML
document, or a PDF document, depending on the file name.

-   `*-deck.md`

    Files with this ending are processed as silde decks. From one source file
    potentially four different targets can be generated:

    -   `*-deck.html` A reveal.js based slide show
    -   `*-handout.hmtl` A HTML document containing the speaker notes to the
        slide show.
    -   `*-deck.pdf` A PDF version of the slide show
    -   `*-handout.pdf` A PDF version of the handout

-   `*-page.md`

    Markdown files ending on `*-page.md` are translated into corresponding HTML
    or PDF documents.

## *decker* targets

-   `decker help`

    Prints this document to stdout in Markdown format.

-   `decker html`

    Builds HTML versions of all available documents.

-   `decker pdf`

    Builds PDF versions of all documents that are generated from
    `*-deck.md` files.

-   `decker pdf-decks`

    Builds PDF versions of all slide decks (requires `decktape.sh`).

-   `decker watch`

    Builds HTML versions of all documents and then watches for document changes.
    Each change to a watched document triggers a rebuild. Watching can be
    terminated with `^C`.

-   `decker server`

    Like `decker watch`. Additionally a local web server is started that serves
    the generated HTML files. The `index.html` document is automatically openend
    in the browser. Changed files are automatically reloaded in the browser. 
    (requires `livereloadx`)

-   `decker example`

    Write a few example files to the current directory. To start exploring decker
    type

    ``` {.bash}
    $ decker example
    $ decker server
    ```

    and make some changes to the Markdown files. `example-deck.md` contains the
    source code for a slide deck that explains most of the features supported.

-   `decker clean`

    Recursively removes all generated files from the current directory.

-   `decker check`

    Check for all required external depencies. If one of the programs is missing,
    an error is generated. Required programs include:

    -   `pdflatex` as part of a complete LaTeX installation
    -   `decktape.sh` for the generation of PDF versions of slide decks
    -   `livereloadx` as live-reloading local webserver
    -   `rsync` to publish the documents to a remote location

-   `decker plan`

    Prints a list of all source files found below the current directory.

-   `decker meta`

    Pretty prints all meta data that can be found in `*.yaml` files in the
    current directory and below. Meta data is mainly used to perform
    substitutions in Markdown documents using the Mustache templating system.

-   `decker publish`

    Publish the generated files to a remote location using `rsync` if the
    location is specified in the meta data. The keys `rsync-destination.host` and
    `rsync-destination.path` specify the publishing destination.

