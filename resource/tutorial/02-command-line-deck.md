---
title: Decker on the command line
---

# Introduction

This slide deck provides an overview over the possible command line arguments supported by decker.

# Workflow

The general recommended workflow of decker on the command line is: 

- `decker example` to create a new project, `cd` into the generated project
- If you have an existing project, navigate to that directory on the command line
- `decker server` to create html versions and open a local server
- Navigate to `localhost:8888` in a browser
- Create and edit `*-deck.md` files and see changes in the browser window on file save
- If finished, shut down the server by pressing `^C/Ctrl C` on the command line

# `decker help`

Prints a help document to stdout in Markdown format.

# `decker info`

Prints information about the current project's directories, the targets (files which will be generated) and the meta data options which are found in the top level `decker.yaml` file. 

# `decker example` and `decker tutorial`

- `decker example` copies an example project to the current directory
- `decker tutorial` copies extended examples and tutorial decks to the current directory

# `decker watch` and `decker server`

- `decker watch` Builds HTML versions of all documents and then watches for document changes. Each change to a watched document triggers a rebuild. Watching can be terminated with `^C/Ctrl C`.

- `decker server`: Like `decker watch`. Additionally a local web server at the address `localhost:8888` is started that serves the generated HTML files. Changed files are reloaded in the browser.

# `decker html` and `decker decks`

- `decker html` creates all HTML files without opening a server
- `decker decks`creates only HTML slide decks

# `decker clean`

- Recursively removes all generated files from the current directory i.e. only the `public` folder. 
- Also clears cached resource folders with version number older than the currently used decker version.

# `decker pdf` and `decker pdf-decks`

- `decker pdf` creates pdf versions of all files
- `decker pdf-decks` creates pdf versions only of the html slide decks

# 

To use `decker pdf` or `decker pdf-decks`, Google Chrome has to be installed.    
- **Windows:** Currently `decker pdf` does not work on Windows. Please add `print: true` or `menu: true` to your slide deck and use the print button on the title slide or in the menu. 
- **MacOS:** Follow the Google Chrome installer instructions. **Google Chrome.app** has to be located in either `/Applications/Google Chrome.app` or `/Users/username/Applications/Google Chrome.app`
Alternatively you can add `chrome` to `$PATH`.  
- **Linux:** `chrome` has to be on `$PATH`.    


# `decker publish`

- Publish the generated files to a remote location using `rsync` if the location is specified in the meta data. 
- The keys `rsync-destination.host` and `rsync-destination.path` specify the publishing destination.
