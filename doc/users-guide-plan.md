# Plan: complete the users' guide and the agent docs

Status: steps 1 to 4 are done, steps 5 to 9 are open.

`users-guide/decker-users-guide.md` has empty sections, placeholder entries and
statements that no longer match the code. The agent topics in
`resource/decker/template/agent-guide/` inherit the gaps, because they point to
the guide for everything they leave out. This plan fills both, step by step.

## Principles

- **Roles.** The guide is the full reference for people. Agent topics are a
  short subset with tested examples, and they must work without the guide.
- **Code first.** Every section is written from the code, then checked against
  a test deck in `test/decks`. Existing notes in `doc/` are drafts to verify,
  not sources to trust.
- **Unverifiable features.** A feature that cannot be shown working loses its
  heading and goes on a list for the maintainer. ThebeLab and Sage are the most
  likely candidates.
- **Diagram examples.** They need external tools (`mmdc`, `d2`) that may be
  missing when tests run. They stay untested unless the tests can skip them when
  a tool is missing.
- **One commit per step.** A step changes the guide and the agent docs together.

## Measuring progress

`test/AgentGuideTests.hs` holds the numbers. Each step lowers them, and step 9
requires all of them to be zero or empty:

- `undocumentedCommands`, `undocumentedOptions`: no heading in the guide yet.
- `emptySectionCount`: headings without any content.
- `todoCount`: `TODO` placeholders.

## Steps

### Step 1: Corrections and guards

Fix what is wrong, without writing new sections.

- Correct wrong statements in the guide (precedence, resource paths, versioning,
  stale keys, broken examples).
- Correct `--help`, `CLAUDE.md` and the readme.
- Link agent topics to the guide on GitHub, so that their pointers lead
  somewhere.
- Add the tests: commands and options have headings, agent pointers name
  existing sections, the numbers above can only go down, and
  `{.decker-example}` blocks in the guide are built.

### Step 2: Configuration

- **Guide:** replace the `TODO` list with a reference grouped by purpose,
  separating core keys from pack-specific ones. Remove the empty "Variables that
  can be defined in the meta data section". Draft:
  `doc/decker-yaml-config-decker.md`.
- **Keys that are missing today:** `decker-plugins`, `page-features`, `chart`,
  `index.*`, `editor.link-prefix`, `external-tools`, the core `template.*` keys
  and the title slide keys (`teaser`, `logos`, `affiliation`).
- **Decided:** `decker-version` is gone from the guide and `versionCheck` from the
  code. It was not called, and it would warn on every project without the key and
  on every version other than the exact one.
- **Agent docs:** expand `config`. `info --json` now also reports
  `decker-plugins`, `page-features` and `codapi.server`, because packs switch them
  and a feature that needs one then fails silently.

Keys that have no effect, left for the maintainer to remove:

- `publish.rsync.options`: still in `replaceLists`, but `decker publish` takes its
  options from `external-tools.rsync.arguments`. `skeleton.yaml` still shows it, and
  also a `publish.destination` that does not exist.
- `mathjax-url`: goes to a Pandoc writer option that no template uses.
- `progress` (top level): `index.progress` is the one that is read.
- `reveal.vertical-slides`: nothing reads it.
- `whisper.options.ffmpeg`: nothing reads it.
- `rpv-test.*` and `test`: test values in `default.yaml`, with their own TODOs.

The base pack has `template.deck.css`, but no `template.page.css`, only the `tudo`
pack does. This is documented as it is.

### Step 3: Slides and layout

- **Guide:** write *Slide layout*: `columns` and `grid` layouts with their areas,
  `.columns-N-M`, `.split`/`.join`, popups, fragments and `.incremental`. Add
  `.notes`, `.handout`/`.deck`/`.comment`, `.details`, `.solution`, `.dupsub`,
  and the built-in macros (`[:fa]`, `[:vspace]`, `[:youtube]` and friends).
- **Macros:** cover the `name:` invocation form and `:(a|b)` alternatives.
- **Decided:** positional macro parameters (`:(1)`) expand now. The pattern in
  `Filter/Template.hs` accepts names that start with a digit.
- **Agent docs:** expand `decks` with tested examples and remove its pointer to
  the guide for layout.

Fixed on the way:

- A code block macro with a one-line template from the front matter inserted the
  whole document (`Filter/Template.hs`).
- A level-3 heading of any three words was treated as column proportions: the
  heading disappeared and the next three elements became columns
  (`Filter/MarioCols.hs`).

Behaviour that is documented as it is, left for the maintainer to decide:

- Content in front of the first area of a `.columns` or `.grid` slide is dropped
  without a diagnostic. `::: left` and images with `.left` do not start an area,
  although `test/decks/column-deck.md` claims they do.
- `.deck`, `.handout`, `.comment` and `.solution` have no effect on divs, spans
  and images, and `.notes` none on spans, although
  `test/decks/handout-deck.md` claims it.
- `::: notes` stays visible in handouts, unlike notes blocks and notes slides.
- `[@name]` link macros fail in a document with a bibliography, because
  `processCites` runs before `expandTemplateMacros`.
- The `.solution` copy that `.dupsub` makes is not removed by `--lecture`, because
  `filterSelectedSlides` runs before `dupSub`. Both slides have the same id.
- `[:youtube]`, which this plan used to list, is not a macro and stays a plain
  link. Web videos are a topic of step 6.
- `width` on a `.details` heading has no effect, although
  `test/decks/detail-deck.md` uses it.

### Step 4: CLI reference

- **Guide:** fill in all targets, commands and options, including `serve`,
  `format`, `handouts`, `pages`, `questions`, `catalog`, `moodle-xml`, `support`,
  `static-files`, `-n`, `-e`, `-t`, `-o`, `-P`, `-l`. Move Chatty out from under
  `publish` into its own chapter.
- **Agent docs:** update the command table in `index`.
- **Readme:** cut the target list down to a link to the guide.
- **Tests:** every `phony` target in `app/Decker.hs` needs a heading.

Fixed on the way:

- `decker pdf` ignored `-m` and used the `-m` values of the previous build
  (`Project/Shake.hs`).
- `decker pdf -p PORT` started the server on `PORT`, but loaded the decks from port
  8888, and failed with `ConnectionClosed` (`app/Decker.hs`).
- The guide said that plain `decker publish` cleans `public/`, leaves out drafts and
  syncs the vector store. Commit `f0b788b4` removed all of that on purpose, so that
  PDFs survive. It also said that the mirror needs `publish.rsync.options`, which
  has no effect, and `--delete` is a default.
- `--help` shows `catalog` and `moodle-xml` with a description, and tells the three
  `info` targets apart.

Behaviour that is documented as it is, left for the maintainer to decide:

- `decker crunch` looks for `x*.webm` next to `x-deck.md`, so the recording of
  `x-deck.md` also contains that of `x-extra-deck.md` in the same directory.
  Matching `x-recording*.webm` would prevent it.
- `decker observed` does the same as `decker support`, because the code that registers
  open pages is commented out in `Server/Server.hs`. Its `watch` flag is not read
  either. The target can go.
- `decker --open` opens the browser before the first build and before the server
  starts (`runTargets`), so the first page load can fail.
- `decker publish` without a destination prints a message, but exits with status 0.
- `decker pdf` loads pages from whatever already listens on the port, which could be
  a server of a different project.
- Plain `decker publish` publishes drafts, and `-l` deletes PDFs and handouts at the
  destination. Neither mode publishes PDFs without drafts.
- The targets `*-handout.pdf` and `*-page.pdf` are computed in `Targets`, but no rule
  builds them.

### Step 5: Getting started

- **Guide:** write Reveal.js, Features, Using Decker, Creating a project and
  Working on a project, built around `decker example`.
- **Agent docs:** add a short project-setup section to `config`.

### Step 6: Media

- **Guide:** write Images, Pdfs, 3D models, Iframes, Videos, Video streams,
  Audio, Graphs and diagrams (`.render` for mermaid, plantuml, dot, gnuplot, d2
  and tex), JS modules, GeoGebra and charts. Fold the GraphViz and Gnuplot
  headings into diagrams. Drafts: `doc/smart-image-urls-page.md`,
  `doc/media-filter-report-page.md`.
- **Agent docs:** new `media` topic. Remove the remaining pointer in `decks`.

### Step 7: Interactive features

- **Guide:** update quizzes (draft: `doc/quiz-page.md`). Write polls and quizzer,
  whiteboard (it changed in October 2026, so check its README against the code),
  feedback, a11y, search, menu, recording and live captions.
- **Agent docs:** new `quizzes` topic. Add a live-code section to `templates`.

### Step 8: Packs and theming

- **Guide:** rewrite Resources and Versioning. Cover palettes, CSS variables,
  `decker-plugins`, `external-tools` and each bundled pack. Drafts:
  `doc/decker-yaml-config-{mario,mono,tudo,jmu-hci-drp}.md`.
- **Agent docs:** review `mono.md`. Add theming to `config`.

### Step 9: Wrap-up

- **Guide:** fill in or remove Conventions, ThebeLab and Sage. Publish the guide
  and link it from `--help` in place of the old wiki link.
- **Agent docs:** switch the pointers from the GitHub URL to the published URL.
- **Tests:** all numbers are zero and the lists are empty.
- **Drafts:** delete the `doc/` drafts that the guide now replaces.
