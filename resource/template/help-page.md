# decker

A markdown based tool for slide deck creation.

## Usage

*decker* behaves very much like a build tool. It works recursively on the current directory and all subdirectories. Markdown files ending on `.md` in those directories are processed and converted to either a reveal.js slide show, a single page HTML document, depending on the file name.

*decker* understands the notion of a *project directory*. The first directory above the current directory that contains a `.git/` directory is assumed to be the project directory. If `.git/` cannot be found, the current directory is used as the project directory. All path calculations are performed relative to the project directory.

The resulting HTML documents and all necessary resources are placed in the `public` directory within the project directory. The content of `public` is self-contained and can directly be served by any static HTML server. No external sites will be referenced during rendering of the slide decks.

`*-deck.md`

:   Files with this ending are processed as silde decks. From one source file potentially four different targets can be generated:

    -   `*-deck.html` A reveal.js based slide show
    -   `*-handout.hmtl` A HTML document containing the speaker notes to the slide show.
    -   `*-deck.pdf` A PDF version of the slide show
    -   `*-handout.pdf` A PDF version of the handout

`*-page.md`

:   Markdown files ending on `*-page.md` are translated into corresponding single page HTML or PDF documents.

## *decker* targets

decker help
:   Prints this document to stdout in Markdown format.

decker html
:   Builds HTML versions of all available documents. This is the default.

decker pdf-decks
:   Builds PDF versions of all slide decks.

decker watch
:   Builds HTML versions of all documents and then watches for document changes. Each change to a watched document triggers a rebuild. Watching can be terminated with `^C`.

decker server
:   Like `decker watch`. Additionally, a local web server is started that serves the generated HTML files. The `index.html` document is automatically openend in the browser. Changed files are automatically reloaded in the browser. The server can be terminated with `^C`.

decker example

:   Write a few example files to the current directory. To start exploring decker type

    ``` {.bash}
    > mkdir first-contact
    > cd first-contact
    > decker example
    > decker server
    ```

    and make some changes to the Markdown files. `example-deck.md` contains the source code for a slide deck that explains most of the features supported.

decker clean
:   Recursively removes all generated files from the current directory.

decker plan
:   Prints a list of all source files found below the current directory.

decker meta
:   Pretty prints all meta data that can be found in `*.yaml` files in the current directory and below. Meta data is mainly used to perform substitutions in Markdown documents using the Mustache templating system.

decker publish
:   Publish the generated files to a remote location using `rsync` if the location is specified in the meta data. The keys `rsync-destination.host` and `rsync-destination.path` specify the publishing destination.
