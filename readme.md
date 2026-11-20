---
lang: en-US
---

[![Tests](https://github.com/decker-edu/decker/actions/workflows/develop.yml/badge.svg)](https://github.com/decker-edu/decker/actions/workflows/develop.yml)
[![Release](https://github.com/decker-edu/decker/actions/workflows/release.yml/badge.svg)](https://github.com/decker-edu/decker/actions/workflows/release.yml)
[![Decker
Page](https://github.com/decker-edu/decker/actions/workflows/jekyll-gh-pages.yml/badge.svg)](https://github.com/decker-edu/decker/actions/workflows/jekyll-gh-pages.yml)

# Decker

A markdown based tool for slide deck creation.

## Installation of binaries

Under [Releases](https://github.com/decker-edu/decker/releases) you can find the binaries of `decker`.

### Installation with Homebrew (macOS and Linux)

On macOS and Linux the easiest way to install `decker` is via [Homebrew](https://brew.sh):

```sh
brew install decker-edu/decker/decker
```

This pulls the latest release binary from the [`decker-edu/homebrew-decker`](https://github.com/decker-edu/homebrew-decker) tap and installs the external tools `decker` needs for its core features (see [External tools](#external-tools)).

The tap offers three install tiers, each building on the previous one:

| Formula        | Installs                                                    |
| -------------- | ---------------------------------------------------------- |
| `decker-naked` | Just the `decker` binary, no external tools                |
| `decker`       | Binary + the common tools (diagrams, mermaid, video)       |
| `decker-full`  | Everything, including TeX Live and whisper.cpp             |

```sh
brew install decker-edu/decker/decker-naked  # binary only
brew install decker-edu/decker/decker        # recommended default
brew install decker-edu/decker/decker-full   # everything
```

Some optional features (PDF export, embedded LaTeX, speech transcription) need additional programs. The `decker` tier documents them in a caveats section that `brew install` prints; the `decker-full` tier installs TeX Live and whisper.cpp automatically and leaves only a Chrome/Chromium install on Linux to you.

To upgrade to a newer release later:

```sh
brew upgrade decker
```

### Manual installation

To install the program by hand you simply need to download the binary and put it in a location where your operating system can find executable files. The executable and its internal dependencies are statically linked and everything `decker` extracts in order to generate its output is inside its binary.

### Manual installation on Windows

If you have already installed `decker` with an installer you can simply replace the `decker.exe` inside its installation directory with a new version.

If you have not yet installed `decker` and want to install one of the released binaries by hand, simply put it into an isolated directory of your choice. You want to rename the executable to `decker.exe`. Then you need to put that directory onto your system's search path for binaries. To do this, search for "environment variables" in Windows' own search bar and pick the option "Edit the system environment variables". Then click the "Environment Variables..." button in the bottom right corner of the opened dialog. In the next dialog window, pick either your user account's or your system's environment variables and search for the entry `Path`. "Edit" the entry, press "New" and enter the location of the directory you put the `decker.exe` exectuable in. Confirm your changes by pressing the "OK" buttons and you are done. If any program you want to run `decker` from was still running, it needs to be restarted now.

### Manual installation on Linux

After you download the `decker`-executable from the Releases rename it to `decker` (or any name of your liking). You may need to set the downloaded file as *executable*. In a terminal, this is done by entering `chmod +x decker` while inside the directory where your `decker`-executable resides.

Now you need to put the executable onto your search path. To see your current search path, enter `echo $PATH` in a terminal. You should see a list of directories that are on your binary search path.

To install `decker` for your user, simply put the downloaded executable into one of the listed directories inside that user's home directory. These are usually `/home/user/.bin`, `/home/user/.local/bin` or `/home/user/bin`. If none of these directories are in your search path you can add a directory of your choice to the search path by adding this to your user's `.profile` configuration file which is also usually located inside that user's home directory:

``` bash
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi
```

If a configuration like this already exists inside your `.profile` config it usually means that the requested directory does not exist yet. Simply create it and move the `decker` executable there, then restart your shell.

To install `decker` for the entire system, simply put it into that system's directory for locally installed binaries which usually is `/usr/local/bin`. You usually need to be *root* in order to perform this operation.

### Manual installation on MacOS

To install `decker` on MacOS you need to download the executable and put it into your binary search path and set the downloaded file as *executable*. 
For instance, download the latest executable from  [Releases](https://github.com/decker-edu/decker/releases) and globally then globally install it on your system:
```
cd ~/Downloads/
mv decker-* decker
chmod +x decker
```
You should then put the decker binary into a directory that is in your PATH (see Linux instructions above).

If you then run `decker -h` in the terminal for the first time, MacOS will show a dialog complaining that it cannot open `decker` for security reasons. Click on "Done", not on "Move to Trash". The open your system settings, go to "Privacy & Security", scroll down to "decker was blocked to protect your Mac". Click "Allow Anyway" and run `decker -h` in the terminal again. In the dialog asking "Open decker?" click on "Open Anyway" and enter the requested password. From that time on you can run `decker` like any other command line programm.

## Installation from source

1.  Install [stack](https://docs.haskellstack.org/en/stable/README/) and
    [Node.js](https://www.npmjs.com/get-npm) (for `npm`)
2.  Clone this repo.
3.  `cd decker`
4.  `git submodule update --init --recursive`
5.  `make install`

Notes:

- Decker will be built using the `language: GHC2021` option for selection of modern haskell language features.
The default version of `stack` installed by your package manager might not be high enough to recognize this option.
You can use `stack upgrade` to ask your `stack` installation to self-update to the latest version.

- Decker will be installed under `~/.local/bin` which is default not recognized by
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

### Dev container

The repository ships a [dev container](https://containers.dev)
(`.devcontainer/`) that provides the complete build environment — the Haskell
toolchain (GHC 9.8.4 to match the `lts-23.28` resolver, `stack`, and Haskell
Language Server), the native tools Decker shells out to (`sassc`, `graphviz`,
`gnuplot`, `rsync`), NodeJS, and the Claude Code CLI. It works with any dev
container client (VS Code, the `devcontainer` CLI, or Zed).

On first start the container installs the toolchain, fetches the git
submodules under `third-party/`, and runs `npm install`. Heavy state
(`~/.stack`, `.stack-work`, `~/.claude`) is kept in named volumes so it
survives rebuilds. Once inside, build and run as usual:

``` sh
stack build -j8
stack test -j1
stack run -- decker --server   # dev server on http://localhost:8888 (forwarded)
```

#### Zed + Podman

Zed opens the dev container natively (it prompts on opening a project that has a
`.devcontainer/devcontainer.json`, or use *Project: Open Remote*). When using
Podman instead of Docker on Apple Silicon, two host-side settings are required:

-   Use the `applehv` VM provider (the default `libkrun` needs a `krunkit`
    binary that is not in Homebrew). In `~/.config/containers/containers.conf`:

    ``` toml
    [machine]
    provider = "applehv"
    ```

-   In Zed's `settings.json`, keep `"use_podman": true` and add
    `"dev_container_use_buildkit": false` (Podman has no BuildKit/buildx).

Avoid driving the Podman VM with two heavy clients at once (e.g. a manual
`podman build` while Zed runs `devcontainer up`), which can wedge the machine.

The `devcontainer` CLI shells out to `docker`, so `docker` must be pointed at
Podman rather than a stopped Docker Desktop. If `devcontainer up` fails with
`Cannot connect to the Docker daemon`, create a Podman context once and select
it:

``` sh
docker context create podman --docker \
    "host=unix://$(podman machine inspect --format '{{.ConnectionInfo.PodmanSocket.Path}}')"
docker context use podman
```

The Podman API socket lives under a volatile `/var/folders/.../T/podman/` path
that changes when the machine is recreated. If the context breaks later,
re-point it with:

``` sh
docker context update podman --docker \
    "host=unix://$(podman machine inspect --format '{{.ConnectionInfo.PodmanSocket.Path}}')"
```

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
    -   Note: The default openSSH implementation on Windows does not work together with
        cygwin's `rsync` implementation. Either move cygwin's ssh implementation higher up in the
        PATH list or run `decker publish` from cygwin's terminal.
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
brew install rsync graphviz gnuplot pdf2svg yarn coreutils pkg-config
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
    -   `*-handout.html` A HTML document containing the speaker notes to the
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
