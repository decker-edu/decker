---
title: Decker on the command line
history: true
---

# Introduction

This slide deck provides an overview over the possible command line arguments supported by decker.

The general recommended workflow of decker on the command line is: 

- `decker example` to create a new project
- `decker server` to create html versions and open a local server
- Navigate to `localhost:8888` in a browser
- Edit the `*.md` files and see changes immediately in the browser window (on file save)
- If finished, shut down the server by pressing `^C/Ctrl C` on the command line
- use `decker pdf` if pdf versions are needed.

# `decker help`

Prints a help document to stdout in Markdown format.

# `decker example` and `decker tutorial`

- `decker example` copies an example project to the current directory
- `decker tutorial` copies extended examples and tutorial decks to the current directory

# `decker watch` and `decker server`

- `decker watch` Builds HTML versions of all documents and then watches for document changes. Each change to a watched document triggers a rebuild. Watching can be terminated with `^C`.

- `decker server`: Like `decker watch`. Additionally a local web server is started that serves the generated HTML files. The `*-deck.html` file is openend in the browser. Changed files are reloaded in the browser.

# `decker html`

`decker html` creates all HTML files without opening a server

# `decker pdf` and `decker pdf-decks`

- `decker pdf` creates pdf versions of all files
- `decker pdf-decks` creates pdf versions only of the html slide decks

To use `decker pdf` or `decker pdf-decks`, Google Chrome has to be installed.    
**Windows:** Follow the Google Chrome installer instructions.  
**MacOS:** Follow the Google Chrome installer instructions. **Google Chrome.app** has to be located in either `/Applications/Google Chrome.app` or `/Users/username/Applications/Google Chrome.app`
Alternatively you can add `chrome` to `$PATH`.  
**Linux:** `chrome` has to be on `$PATH`.    

# `decker clean`

- Recursively removes all generated files from the current directory. 
- Also clears cached resource folders with version number older than the currently used decker version.

# `decker plan`

Prints a list of all source files found below the current directory.

# `decker meta`

- Pretty prints all meta data that can be found in `*.yaml` files in the current directory and below. 
- Meta data is mainly used to perform substitutions in Markdown documents using the Mustache templating system.

# `decker publish`

- Publish the generated files to a remote location using `rsync` if the location is specified in the meta data. 
- The keys `rsync-destination.host` and `rsync-destination.path` specify the publishing destination.
