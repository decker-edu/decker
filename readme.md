[![build status](https://cgmgit.beuth-hochschule.de/teaching/decker/badges/master/build.svg)](https://cgmgit.beuth-hochschule.de/teaching/decker/commits/master)

# decker

A markdown based tool for slide deck creation.

## Installation

Pick a [published release](https://cgmgit.beuth-hochschule.de/teaching/decker/tags), download and unpack:

``` {.sh}
gunzip decker.gz
chmod a+x decker
```

## Installation from source

1.  Install [stack](https://docs.haskellstack.org/en/stable/README/).
2.  Clone this repo.
3.  `cd decker`
3.  `stack setup`
4.  `stack install`

## External tools

Decker uses a few external tools that need to be installed on the system:

- [*rsync*](http://formulae.brew.sh/repos/Homebrew/homebrew-core/formula/rsync) for publishing slide decks and resources
- [*unzip*](http://formulae.brew.sh/repos/Homebrew/homebrew-core/formula/unzip) to extract resources from the decker executable
- [*decktape*](https://github.com/astefanutti/decktape) to convert HTML slide decks to PDF format
 
## Usage

*decker* behaves very much like a build tool. It works recursively on the current directory and all subdirectories. Markdown files ending on `.md` in those directories are processed and converted to either a reveal.js slide show, a HTML document, or a PDF document, depending on the file name.

-   `*-deck.md`

    Files with this ending are processed as silde decks. From one source file potentially four different targets can be generated:

    -   `*-deck.html` A reveal.js based slide show
    -   `*-handout.hmtl` A HTML document containing the speaker notes to the slide show.
    -   `*-deck.pdf` A PDF version of the slide show
    -   `*-handout.pdf` A PDF version of the handout

-   `*-page.md`

    Markdown files ending on `*-page.md` are translated into corresponding HTML or PDF documents.

## *decker* targets

-   `decker help`

    Prints a help document to stdout in Markdown format.

-   `decker html`

    Builds HTML versions of all available documents.

-   `decker pdf`

    Builds PDF versions of all documents that are generated from `*-page.md` files.

-   `decker pdf-decks`

    Builds PDF versions of all slide decks (requires `decktape.sh`).

-   `decker watch`

    Builds HTML versions of all documents and then watches for document changes. Each change to a watched document triggers a rebuild. Watching can be terminated with `^C`.

-   `decker server`

    Like `decker watch`. Additionally a local web server is started that serves the generated HTML files. The `*-deck.html` file is openend in the browser. Changed files are reloaded in the browser. (still requires `livereloadx`)

-   `decker example`

    Write a few example files to the current directory. To start exploring decker type

    ``` {.bash}
    $ decker example
    $ decker server
    ```

    and make some changes to the Markdown files. `example-deck.md` contains the source code for a slide deck that is supposed to (someday) explain most of the features supported.

-   `decker clean`

    Recursively removes all generated files from the current directory.

-   `decker check`

    Check for all required external depencies. If one of the programs is missing, an error is generated. Required programs include:

    -   `pdflatex` as part of a complete LaTeX installation
    -   `decktape.sh` for the generation of PDF versions of slide decks
    -   `livereloadx` as live-reloading local webserver
    -   `rsync` to publish the documents to a remote location

-   `decker plan`

    Prints a list of all source files found below the current directory.

-   `decker meta`

    Pretty prints all meta data that can be found in `*.yaml` files in the current directory and below. Meta data is mainly used to perform substitutions in Markdown documents using the Mustache templating system.

-   `decker publish`

    Publish the generated files to a remote location using `rsync` if the location is specified in the meta data. The keys `rsync-destination.host` and `rsync-destination.path` specify the publishing destination.

## Contributions

### Pull requests

Contributions are accepted via pull requests. Before working on a feature, please write up an issue and discuss it with the other developers.

### CI build checks

The decker repository has a GitLab CI runner configured, that builds and runs all tests for each commit on every branch. Look at the status display for recent run of the [CI pipelines](pipelines).

### Haskell source code formatting

Haskell soure code readability depends heavily on consistent formatting conventions. With decker, formatting is automated using the excellent [hindent]() tool. Formatting is checked for each commit that is uploaded to the GitLab repository.
