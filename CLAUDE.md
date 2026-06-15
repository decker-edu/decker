# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What is Decker

Decker is a Haskell-based tool that converts Markdown files into Reveal.js slide decks, HTML pages, and handouts. It uses Pandoc for Markdown processing, Shake as a build system, and bundles its own templates and support resources.

## Build Commands

```sh
stack build -j8                    # Build the project
stack test -j1                     # Run tests
stack test -j1 --file-watch        # Run tests on file change
stack run -- decker --server       # Run decker with live reload server
make install                       # Clean build + install to ~/.local/bin
make build                         # Quick build without clean
make unclean-install               # Build + install without cleaning first
```

The project uses `stack` with resolver `lts-23.28` and `language: GHC2021`. The cabal file is generated from `package.yaml` via hpack.

To build with embedded resources (for distribution): `ATTACH_RESOURCE_ZIP=1 stack build -j8`

## Architecture

### Build System (Shake-based)

The entry point is `app/Decker.hs` which calls `runDecker` from `Text.Decker.Project.Shake`. Decker uses Shake as both its internal build system and CLI framework. Build rules in `Decker.hs` define how source files map to outputs:

- `*-deck.md` → `*-deck.html` (slide deck), `*-handout.html`, `*-deck.pdf`
- `*-page.md` → `*-page.html`
- `*-quest.md` → `*-quest.html` (exam questions)

The `Targets` type in `Text.Decker.Project.Project` tracks all source-to-output mappings via lenses (`decks`, `pages`, `handouts`, `questions`, `static`, etc.).

### Filter Pipeline

Pandoc AST filters in `src/Text/Decker/Filter/` transform the document between parsing and rendering. Key filters:
- `Filter.hs` — orchestrates the filter pipeline via `processPandoc`/`processSlides`
- `Decker2.hs` — main media/content filter
- `Layout.hs` — slide layout processing
- `MarioCols.hs` — multi-column layouts
- `Slide.hs` — slide splitting and structuring
- `Monad.hs` — filter monad for stateful transformations

Filters run within `Decker`, a `StateT DeckerState Action` monad combining Shake actions with filter state.

### Resource System

Resources (templates, CSS, JS, vendor libs) live in `resource/decker/`. Templates are in `resource/decker/template/`, support files in `resource/decker/support/`. At build time, resources can be embedded into the binary via a zip archive (see `Setup.hs`, `Text.Decker.Resource.Zip`). Third-party dependencies (Reveal.js, MathJax, Font-Awesome) are git submodules under `third-party/`.

### Key Modules

- `Text.Decker.Project.Shake` — Shake runner, CLI arg parsing, file watching, dev server
- `Text.Decker.Project.Project` — project scanning, target discovery
- `Text.Decker.Internal.Common` — core types (`Disposition`, `Layout`, `OutputFormat`, `Decker` monad)
- `Text.Decker.Internal.Meta` — YAML metadata handling
- `Text.Decker.Writer.Html` — HTML output generation
- `Text.Decker.Reader.Markdown` — Markdown reading with decker extensions
- `Text.Decker.Internal.Caches` — template and metadata caching
- `Text.Decker.Server.Server` — development HTTP server with WebSocket reload

### Conventions

- Default language extensions: `OverloadedStrings`, `QuasiQuotes`, `MultiWayIf` (set in cabal)
- Many modules use `NoImplicitPrelude` with `Relude` as the alternative prelude
- Source formatting: [ormolu](https://github.com/tweag/ormolu) via Haskell Language Server
- Output goes to `public/` directory; support resources to `public/support/`
- Project config via `decker.yaml` in the project root

### Third-Party Resources

After updating git submodules, run `make upgrade-third-party` to copy vendor resources into `resource/decker/support/vendor`.
