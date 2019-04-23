[![pipeline status](https://gitlab2.informatik.uni-wuerzburg.de/decker/decker/badges/master/pipeline.svg)](https://gitlab2.informatik.uni-wuerzburg.de/decker/decker/commits/master)

# decker

A markdown based tool for slide deck creation.

## Installation

Pick a [published release](), download and unpack:

``` {.sh}
gunzip decker.gz
chmod a+x decker
```

## Installation from source

1.  Install [stack](https://docs.haskellstack.org/en/stable/README/).
2.  Clone this repo.
3.  `cd decker`
4.  `make install`

## External tools

Decker uses a few external tools that need to be installed on the system:

-   [*ssh*](https://www.openssh.com) for publishing slide decks and resources
-   [*rsync*](http://formulae.brew.sh/repos/Homebrew/homebrew-core/formula/rsync)
    for publishing slide decks and resources
-   [*unzip*](http://formulae.brew.sh/repos/Homebrew/homebrew-core/formula/unzip)
    to extract resources from the decker executable
-   [*LaTeX* with pdflatex](https://www.latex-project.org) to generate LaTeX in
    PDF-files and embedded Tikz figures
-   [*Graphviz*](http://graphviz.org) to generate graphs using `dot`
-   [*Gnuplot*](http://gnuplot.sourceforge.net) to generate graphs using `dot`
-   [*pdf2svg*](https://github.com/dawbarton/pdf2svg) to generate SVG files from
    PDF documents
-   *libbzip2-dev*
-   [*NodeJS*](https://nodejs.org/) as a prerequisite for Yarn
-   [*Yarn*](https://yarnpkg.com) to install Javascript dependencies

### Installation of external tools on macOS

Use [Homebrew](https://brew.sh) to install most of them.

``` {.sh}
brew install rsync unzip graphviz gnuplot pdf2svg yarn
```

For the rest follow instructions on their respective webites.

## Usage

*decker* behaves very much like a build tool. It works recursively on the
current directory and all subdirectories. Markdown files ending on `.md` in
those directories are processed and converted to either a reveal.js slide show,
a HTML document, or a PDF document, depending on the file name.

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

### Docker container

We provide prebuild docker containers. You may use them in a directory to build the html slides with 

```
docker run --rm -it -v `pwd`:/decker -p 8888:8888 gitlab2.informatik.uni-wuerzburg.de:4567/decker/decker html
```

or for Windows

```
docker run --rm -it -v %cd%:/decker -p 8888:8888 gitlab2.informatik.uni-wuerzburg.de:4567/decker/decker html
```

Exchange the `html` at the end of the command with your *decker* command of choice. Beware that file updates are not propagated into the container so `decker server` will not auto refresh.

## *decker* targets

-   `decker version`

    Prints the current decker version and branch as well as the current pandoc version.

-   `decker help`

    Prints a help document to stdout in Markdown format.

-   `decker info`

    Prints information about the current project's directories, the targets (files which will be generated) and the meta data options which are found in top level `*-meta.yaml` files. 

-   `decker html`

    Builds HTML versions of all available documents.

-   `decker decks`

    Builds only HTML slide decks.

-   `decker pdf`

    Builds PDF versions of all documents. 

-   `decker pdf-decks`

    Builds PDF versions of all slide decks.

    To use `decker pdf` or `decker pdf-decks`, Google Chrome has to be installed.    
    **Windows:** Currently `decker pdf` does not work on Windows. Please add `print: true` or `menu: true` to your slide deck and use the print button in the menu or on the title slide.
    **MacOS:** Follow the Google Chrome installer instructions. **Google Chrome.app** has to be located in either `/Applications/Google Chrome.app` or `/Users/username/Applications/Google Chrome.app`
    Alternatively you can add `chrome` to `$PATH`.  
    **Linux:** `chrome` has to be on `$PATH`.    

-   `decker watch`

    Builds HTML versions of all documents and then watches for document changes.
    Each change to a watched document triggers a rebuild. Watching can be
    terminated with `^C`.

-   `decker server`

    Like `decker watch`. Additionally a local web server is started that serves
    the generated HTML files. The `*-deck.html` file is openend in the browser.
    Changed files are reloaded in the browser. (still requires `livereloadx`)

-   `decker example`

    Write a few example files to the current directory. To start exploring
    decker type

    ``` {.bash}
    $ decker example
    $ decker server
    ```

    and make some changes to the Markdown files. `example-deck.md` contains the
    source code for a slide deck that explains most of the available features for creating slide decks.

-   `decker tutorial` 

    Like `example` but copies extended example/tutorial slide decks to the current directory.

-   `decker clean`

    Recursively removes all generated files from the current directory (i.e. the `public` folder).
    Also removes cached resources witch version number lower than the current version.

-   `decker publish`

    Publish the generated files to a remote location using `rsync` if the
    location is specified in the meta data. The keys `rsync-destination.host`
    and `rsync-destination.path` specify the publishing destination.

## Contributions

### Pull requests

Contributions are accepted via pull requests. Before working on a feature,
please write up an issue and discuss it with the other developers. 
For each implemented feature, increment the version number in `package.yaml`. 
Breaking changes increment the second number. Fixes increment the third number.

### CI build checks

The decker repository has a GitLab CI runner configured, that builds and runs
all tests for each commit on every branch. Look at the status display for recent
run of the [CI pipelines](pipelines).

### Haskell source code formatting

Haskell soure code readability depends heavily on consistent formatting
conventions. With decker, formatting is automated using the excellent
[hindent]() tool. Formatting is checked for each commit that is uploaded to the
GitLab repository.

## Compile Flags
The Decker executable contains per default all necessary supporting files and extracts them on the first run. Some packaging solutions prefer to already extract the files during the installation. To support this, a compile flag `preextractedresources` is available which instructs Decker to work with the already extracted resource files. Invoke `stack --flag decker:preextractedresources` to compile such a version.