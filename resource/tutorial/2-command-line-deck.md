# Decker Command Line

# Overview over the decker command line

If you can read this, you probably already know how to use decker on the command line but here is an additional overview over all possible command line arguments supported by decker.

# decker example & decker tutorial

- `decker example` copies an example project to the current directory
- `decker tutorial` copies extended examples and tutorial decks to the current directory

# `decker watch` and `decker server`

- `decker watch` Builds HTML versions of all documents and then watches for document changes. Each change to a watched document triggers a rebuild. Watching can be terminated with `^C`.

- `decker server`: Like `decker watch`. Additionally a local web server is started that serves the generated HTML files. The `*-deck.html` file is openend in the browser. Changed files are reloaded in the browser. (still requires `livereloadx`)

# decker html

- `decker html` creates html files without opening a server

# decker pdf & decker pdf-decks

- `decker pdf` creates pdf files of all files
- `decker pdf-decks` creates pdf files only of the slide decks

To use `decker pdf` or `decker pdf-decks`, Google Chrome has to be installed.    
**Windows:** Follow the Google Chrome installer instructions.  
**MacOS:** Follow the Google Chrome installer instructions. **Google Chrome.app** has to be located in either `/Applications/Google Chrome.app` or `/Users/username/Applications/Google Chrome.app`
Alternatively you can add `chrome` to `$PATH`.  
**Linux:** `chrome` has to be on `$PATH`.    

# `decker clean`

    Recursively removes all generated files from the current directory.

# `decker plan`

    Prints a list of all source files found below the current directory.

# `decker meta`

    Pretty prints all meta data that can be found in `*.yaml` files in the
    current directory and below. Meta data is mainly used to perform
    substitutions in Markdown documents using the Mustache templating system.

# `decker publish`

    Publish the generated files to a remote location using `rsync` if the
    location is specified in the meta data. The keys `rsync-destination.host`
    and `rsync-destination.path` specify the publishing destination.
