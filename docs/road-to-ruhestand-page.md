# Features

## Build System

### Public folder

- [ ] compile to `public/`
- [ ] select decks for publishing
- [ ] publish only `public/`
- filter list for published content
- compile in place
- explicit staging to `public/`
- explicit publishing to server

### Dependencies

- ? dynamic transient dependencies
- ? watch only .md files
- ? handle `[:include]`, store dependencies to JSON files

### Targets

- info
- support
- publish
- clean
- serve
- crunch
- transcribe
- pdf
- compile
- search-info
- observed

## Compiler

### Filter

- [x] rendered code blocks
  - [x] configure external filters
    - mermaid
    - plantuml
    - dot
    - gnuplot
    - d2
    - latex
    - javascript
    - comment
    - sagemath
- [x] media filtering
  - [ ] youtube streaming
  - [x] homogenous layout
- [x] image tag handling
  - [x] images
  - [x] video
  - [x] rendered code
  - [x] include source code
  - [x] iframes
  - [x] mview / modelviewer
  - [x] geogebra
  - [x] audio
  - [x] pdf
  - [x] embedded svg
  - [ ] streaming
- [x] H2 blocks
  - generate divs with classes
  - column and grid layout
  - detail boxes
- [x] mario columns syntax
- [x] detail filter
- [x] vspace, hspace
- [x] macro templates
- [x] fragment templates
  - codeblock, link, and image tags
  - used for: parameterized snippets, codapi
  - extend match params from name to attributes

### Templating

- [ ] Pandoc templates
- [x] Other templates (jinja2, handlebars, jsx, ask Ugo)

### Index Generation

- [x] collect and render index info to meta data for index template
- [x] live-searchable reverse index for fuzzy search

## Server

- [x] upload annotations, times, videos
- [x] video snippet assembly
- [x] serve public on localhost
- [x] reload on compile
