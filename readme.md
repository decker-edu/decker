---
lang: en-US
---

[![Tests](https://github.com/decker-edu/decker/actions/workflows/develop.yml/badge.svg)](https://github.com/decker-edu/decker/actions/workflows/develop.yml)
[![Release](https://github.com/decker-edu/decker/actions/workflows/release.yml/badge.svg)](https://github.com/decker-edu/decker/actions/workflows/release.yml)
[![Decker
Page](https://github.com/decker-edu/decker/actions/workflows/jekyll-gh-pages.yml/badge.svg)](https://github.com/decker-edu/decker/actions/workflows/jekyll-gh-pages.yml)

# Decker

A markdown based tool for slide deck creation.

## Installation from source

1.  Install [stack](https://docs.haskellstack.org/en/stable/README/) and
    [Node.js](https://www.npmjs.com/get-npm) (for `npm`)
2.  Clone this repo.
3.  `cd decker`
4.  `git submodule update --init --recursive`
5.  `make install`

Note:

Decker will be installed under `~/.local/bin` which is default not recognized by
your terminal. If decker is not found by your terminal, add the path to the
corresponding config file. For zsh (default for macos) do the following steps.
Run from the terminal:

1.  `touch ~/.zshrc`
2.  `echo PATH=$HOME/.local/bin:$PATH > ~/.zshrc`
3.  `source ~/.zshrc`

### Third-party resources

`reveal.js` `MathJax` and `Font-Awesome` dependencies are tracked via
submodules. After upgrading any of these submodules you need to run

``` sh
make upgrade-third-party
```

to copy the needed resources into the main repo at
`resource/decker/support/vendor`.

## Installation from source on Windows

Instead of a `makefile` we use a PowerShell script on Windows to install decker
from source

1.  `cd decker`
2.  `.\bin\build.ps1`

If you want to copy `decker` to `C:\Program Files (x86)` you can call
`.\bin\build.ps1 -local`. This needs a PowerShell session with administrator
rights.

To then call decker from anywhere on the PowerShell command line create a
PowerShell profile file, add the following line, and restart your PowerShell
session!

`$Env:Path += ";${Env:ProgramFiles(x86)}\Decker\bin"`

### Note:

Windows Antivirus Protection has a high impact on compilation time. Add the
following directories as exclusions to safe about 20-40% compilation time.

-   Haskell stack build tool: usually under `C:\sr`
-   Haskell compiler:
    `%AppData%\Local\Programs\stack\x86_64-windows\ghc-x.x.x\bin`
-   this repository

## Development

### Haskell

Use appropriate tooling. I use:

-   *Visual Studio Code* with the following plugins:
    -   *Haskell Language Server*
    -   *hindent-format*

### Templates and CSS

To interactively work on the template, CSS and Javascript files in
`resource/template` and `resource/support` run Decker as
`stack run decker server`. This will automatically incorporate all changes and
reload the documents in the browser.

## External tools

Decker uses a few external tools that need to be installed on the system to use
the full functionality:

-   [*ssh*](https://www.openssh.com) for publishing slide decks and resources
-   [*rsync*](http://formulae.brew.sh/repos/Homebrew/homebrew-core/formula/rsync)
    for publishing slide decks and resources
    -   Note: openssh Server do not work properly with rsync for Windows. Use
        cygwin and its terminal to perform decker publish.
-   [*LaTeX* with pdflatex](https://www.latex-project.org) to generate LaTeX in
    PDF-files and embedded Tikz figures
-   [*Graphviz*](http://graphviz.org) to generate graphs using `dot`
-   [*Gnuplot*](http://gnuplot.sourceforge.net) to generate graphs using `dot`
-   [*pdf2svg*](https://github.com/dawbarton/pdf2svg) to generate SVG files from
    PDF documents
-   *libbzip2-dev*
-   [*NodeJS*](https://nodejs.org/) to install JavaScript dependencies
-   [*coreutils*](https://www.gnu.org/software/coreutils/) the GNU coreutils

### Installation of external tools on macOS

Use [Homebrew](https://brew.sh) to install most of them.

``` sh
brew install rsync graphviz gnuplot pdf2svg yarn coreutils
```

For the rest follow instructions on their respective webites.

To confirm that you have installed all of the required external tools, run the
following command in a terminal window:

`decker check`

### Installation of external tools on Linux

Use [Ubuntu's Advanced Packaging Tool
(APT)](https://ubuntu.com/server/docs/package-management) to install external
tools.

``` sh
apt-get update && apt-get install -y texlive-full plantuml gnuplot graphviz libbz2-dev pdf2svg rsync ssh libtinfo-dev libgmp3-dev zlib1g-dev
```

To confirm that you have installed all of the required external tools, run the
following command in a terminal window:

`decker check`

## Usage

*Decker* behaves very much like a build tool. It works recursively on the
current directory and all subdirectories. Markdown files ending on `.md` in
those directories are processed and converted to either a
[Reveal.js](https://revealjs.com) slide show, a HTML document, or a PDF
document, depending on the file name.

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

## *Decker* targets

-   `decker version`

    Prints the current Decker version and branch as well as the current pandoc
    version.

-   `decker help`

    Prints a help document to stdout in Markdown format.

-   `decker info`

    Prints information about the current project's directories, the targets
    (files which will be generated) and the meta data options which are found in
    top level `decker.yaml` file.

-   `decker html`

    Builds HTML versions of all available documents.

-   `decker decks`

    Builds only HTML slide decks.

-   `decker pdf`

    Builds PDF versions of all documents.

-   `decker pdf-decks`

    Builds PDF versions of all slide decks.

    To use `decker pdf` or `decker pdf-decks`, Google Chrome has to be
    installed.\
    **Windows:** Currently `decker pdf` does not work on Windows. Please add
    `print: true` or `menu: true` to your slide deck and use the print button in
    the menu or on the title slide. **MacOS:** Follow the Google Chrome
    installer instructions. **Google Chrome.app** has to be located in either
    `/Applications/Google Chrome.app` or
    `/Users/username/Applications/Google Chrome.app` Alternatively you can add
    `chrome` to `$PATH`.\
    **Linux:** `chrome` has to be on `$PATH`.

-   `decker --watch`

    Builds HTML versions of all documents and then watches for document changes.
    Each change to a watched document triggers a rebuild. Watching can be
    terminated with `^C`.

-   `decker --server`

    Like `decker watch`. Additionally a local web server is started that serves
    the generated HTML files. The `*-deck.html` file is openend in the browser.
    Changed files are reloaded in the browser. (still requires `livereloadx`)

-   `decker example`

    Write a few example files to the current directory. To start exploring
    Decker type

    ``` bash
    $ decker example
    $ cd example
    $ decker --server
    ```

    and make some changes to the Markdown files.

-   `decker clean`

    Recursively removes all generated files from the current directory (i.e. the
    `public` folder). Also removes cached resources.

-   `decker publish`

    Publish the generated files to a remote location using `rsync` if the
    location is specified in the meta data. The keys `rsync-destination.host`
    and `rsync-destination.path` specify the publishing destination.

## Contributions

### Pull requests

Contributions are accepted via pull requests. Before working on a feature,
please write up an issue and discuss it with the other developers. For each
implemented feature, increment the version number in `package.yaml`. Breaking
changes increment the second number. Fixes increment the third number.

### Tooling

Use appropriate tooling. We use [Visual Studio
Code](https://code.visualstudio.com) with the *Haskell Language Server* plugin.

### Haskell source code formatting

Haskell soure code readability depends heavily on consistent formatting
conventions. Formatting is automated using the excellent
[ormolu](https://github.com/tweag/ormolu) formatter via the [Haskell Language
Server](https://github.com/haskell/haskell-language-server).

## License

See [COPYING](./COPYING).

## License

See [COPYING](./COPYING).
