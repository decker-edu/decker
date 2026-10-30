# Plan: complete the users' guide and the agent docs

Status: all steps are done. What is left is on the lists for the maintainer.

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
- **Diagram examples.** They need external tools that may be missing when tests
  run. An example with `requires="dot"` is skipped without the program and has to
  run without an error with it. Mermaid and LaTeX examples are not tested: `mmdc`
  also needs a browser that it can find, and LaTeX the `standalone` class.
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
- **Tests:** `decker example` writes a project that builds without diagnostics.

Behaviour that is documented as it is, left for the maintainer to decide:

- A project without `decker.yaml` fails with Shake's `file does not exist and no rule
  available`, which does not tell a new user what to do. This includes a `.git`
  directory without `decker.yaml`.
- `decker example` writes below the project root, and the outer project then builds
  the example as well, because a nested `decker.yaml` does not stop the search.
- The output of a removed source stays in `public/` and gets published. Only the index
  page drops it.
- `lang` defaults to `de` in the base pack, so a new project has German labels. The
  example project does not set it either.
- In a deck, the keys `P` and `H` of Reveal.js do nothing on a single press, because
  Decker binds them to triple clicks. `?` still lists what Decker binds.

### Step 6: Media

- **Guide:** write Images, Pdfs, 3D models, Iframes, Videos, Video streams,
  Audio, Graphs and diagrams (`.render` for mermaid, plantuml, dot, gnuplot, d2
  and tex), JS modules, GeoGebra and charts. Fold the GraphViz and Gnuplot
  headings into diagrams. Drafts: `doc/smart-image-urls-page.md`,
  `doc/media-filter-report-page.md`.
- **Agent docs:** new `media` topic. Remove the remaining pointer in `decks`.

Fixed on the way:

- Videos, iframes, streams, 3D models and GeoGebra applets stayed empty on pages.
  They only have a `data-src`, and nothing on a page loads it. `support/js/page.js`
  would, but no template includes it and it calls `setAttribute` on the wrong object.
  The filter now sets `src` on pages (`lazySrc` in `Filter/Media.hs`), so this file
  can go.

Checked in Chrome: images, rows, SVG, PDF, iframes, video, audio, both 3D viewers,
GeoGebra, all diagram types, modules and charts. Streams were only checked as far as
their HTML. In the test, the YouTube frame stayed black and Twitch refused.

Behaviour that is documented as it is, left for the maintainer to decide:

- A failing diagram program does not fail the build, not even with `--strict`. There
  is no diagnostic, only `# ERROR:` on stdout and an SVG with the message. An agent
  sees exit 0. It also stays after the program is installed, until `decker clean`.
- `chart.defaults.plugins.colorschemes` in `default.yaml` has no effect, because the
  plugin is not part of the vendored Chart.js.
- Charts stay CSV text in handouts and on pages, since only `deck-plugins.html` loads
  the plugin.
- `align` only works on images. On everything else `data-align` ends up on another
  element than the one `media.css` looks at. `test/decks/media-deck.md` and
  `media-page.md` use it on diagrams and code.
- `color` on a background image has no effect, although
  `test/decks/background-image-deck.md` uses it.
- A background video, iframe or stream in a handout or on a page goes through
  `imageBlock`: Pandoc makes `<video>` or `<embed>` of it, and for a stream an `<img>`
  with `src="youtube:..."`, which shows nothing.
- `.log` replaces the scales of `.stackedBar-chart`, so the bars are no longer stacked.
- `.tex` leaves its `.aux` and `.log` files in `public/.rendered-code`, and they get
  published. The diagram sources are published as well.
- Twitch embeds send a fixed `parent=localhost`, so the player refuses on any other
  host, including `127.0.0.1`.
- `veer` and `veer-photo` are stream schemes in `classifyMedia`, but `streamBlock` fails
  on them with `Unsupported stream service`.
- An unknown snippet name (`![](f.py#nope){.code}`) gives an empty block, without a
  diagnostic.
- `![](x.pdf){.image}` and the like do not give an `<img>`. Pandoc chooses the element
  from the extension again, and writes `<embed>` or `<video>`.
- `Filter/MarioMedia.hs` is imported nowhere and can go.

### Step 7: Interactive features

- **Guide:** update quizzes (draft: `doc/quiz-page.md`). Write polls and quizzer,
  whiteboard (it changed in October 2026, so check its README against the code),
  feedback, a11y, search, menu, recording and live captions.
- **Agent docs:** new `quizzes` topic. Add a live-code section to `templates`.

The guide has a new chapter *During a presentation*. *Whiteboard* and *Audience
Feedback* moved there.

Fixed on the way:

- A YAML block in a question was shown on the slide and had no effect, because the
  media filter wraps code blocks before `handleQuizzes` runs (`Filter/Quiz.hs`).
- The `W` key had no description in the help (`whitebard`), and the whiteboard toggle
  button no tooltip (`whiteboard.js`).
- The guide said that a list with one item in `.qic` becomes a blank (it is a selection
  box with one option), and suggested `- B` as a wrong answer in `.qft` (every item of
  a plain list is correct). It also named `print: true` and `menu: true`, which do
  nothing. The print button is always in the menu.

Checked in Chrome: all quiz types, the whiteboard with saving and auto-save, a11y,
search, menu, and recording up to the upload, with fake devices. The live quizzer was
checked up to the start of a poll with QR code on `quiz.jetzt`, without voters, so its
results were not seen. Polls on exam questions were checked up to the session on
`tramberend.bht-berlin.de`. Feedback was checked up to the button, with the server of TU
Dortmund. Live captions were checked up to the button, because headless Chrome hears
nothing. Green screen and playback are from the code only.

**Removed from the guide, because it cannot be shown working:** the `poll` plugin (`poll:
true`, `{.poll}` on a slide). `poll.js` has `polls.hci.informatik.uni-wuerzburg.de` hard
coded, and it timed out on 2026-10-30. The overlay stays on its loading animation. Also,
its YAML block has no effect and the labels of its chart are empty, because
`Filter/Poll.hs` expects a slide structure that changed. `test/decks/poll-deck.md` shows
it. The plugin, `Filter/Poll.hs` and the deck can go, or the plugin needs a server.

Behaviour that is documented as it is, left for the maintainer to decide:

- `decker-plugins.quiz` loads `quiz/quiz.js`, but `deck-plugins.html` never adds it to
  `plugins`, so it does not start. Only its top-level code runs. `.quiz` lists
  (`test/decks/ar-deck.md`), `polling.server`, `polling.selection` and `disable-quizzes`
  have no effect. The plugin can go. `decker-plugins.quiz-wue` is the one that matters.
- In a handout, the answers of a quiz are visible as checked boxes, which also applies to
  pages. `deckerPipeline` only runs `handleQuizzes` for decks.
- `quiz.solution`, on a document or a question, only adds its value as a class to the
  question. `score`, `category`, `lectureId` and `topic` are parsed and never used.
- `Show Solution` on a matching question without any moved item gives a browser `alert`.
- The flash message of a11y mode says *Accessible Colors*, but no CSS looks at the class
  `a11y`, so no color changes.
- Presenter mode tries to open a poll session in every deck, also without `poll-server`,
  and logs `[Examiner][Poll] Cannot connect` on the console.
- `Reveal.getConfig().menu` is passed to the menu plugin, which does not read it.
- A second live quiz on a slide is only an error message in the browser. The build does
  not report it.
- The quizzer uses `http://localhost:3000/` without `quizzer.url`, and `mono` only sets
  `poll-server`. So with `mono`, a live quiz needs `quizzer.url` in the project.
- A question without a level 1 heading above it breaks the slide structure, without a
  diagnostic.
- The whiteboard saves into the downloads folder and to the server on each save, so a
  presenter gets a new download per save and page leave.
- `tudo`, `nwburg` and `mario` load the quizzer in their own templates, so
  `decker-plugins.quizzer` has no effect there.

### Step 8: Packs and theming

- **Guide:** rewrite Resources and Versioning. Cover palettes, CSS variables,
  `decker-plugins`, `external-tools` and each bundled pack. Drafts:
  `doc/decker-yaml-config-{mario,mono,tudo,jmu-hci-drp}.md`.
- **Agent docs:** review `mono.md`. Add theming to `config`.

Corrected on the way: the guide said that any failure to load a pack terminates Decker
and that `decker purge` clears a pack cache (there is no cache). The guide and `config`
used `--font-size-base` as the example for `css-variables`, which only `nwburg` reads.
The drafts wrote `resource-pack: mono`, which looks for a directory.

Checked in Chrome: decks of `mono`, `mario` and `nwburg` with blocks and accents, and the
computed values of the palette and CSS variables in both color modes. Decks of `tudo` and
`wburg` were only seen without blocks and accents. Pages and the index of the packs were
built, but not looked at. That quizzes react in `mono` with `quiz-wue: true` is only
checked as far as the plugin being imported. Local packs were also built with an installed
Decker, because a development run does not copy support files. The draft for
`jmu-hci-drp` is not used: that pack is not part of this repository, and it could not
be checked.

Behaviour that is documented as it is, left for the maintainer to decide:

- `wburg/template/handout.html` does not compile (`unexpected end of input`), so `wburg`
  cannot build handouts. Its pages and handouts also use `css-color-declarations`, which
  nothing sets. `wburg` and `nwburg` can go if `jmu-hci-drp` replaces them.
- `decker-plugins` and `page-features` only work with the base templates, but `decker info
  --json` reports them for all packs, and an agent is told to trust that output.
- `extractFast` in `app/Decker.hs` ignores `LocalZip`, so the support files of a ZIP pack
  are not copied. It also uses `parseRelDir`, so an absolute pack path fails, and
  `copyDirRecur`, so a pack without `support/` fails. A development run has none of these
  failures, because it serves support files directly.
- Support files are copied once per commit of Decker, so changes to a directory pack need
  `decker clean`. Selecting another pack keeps the files of the old one.
- A URL as `resource-pack` is ignored without a message (`parseSourceURI` returns `None`).
- `mono` publishes absolute source paths through `experiments.add-document-path`, unless
  `--lecture` is used.
- `mono`'s `support/js/mono.js`, `block-manip.js` and `block-manip.css` are not loaded by
  anything. `poll-server` in `mono` points to a personal server.
- `resource/decker/template/palette/*.yaml` is not read. The files also use the old
  `palette.colors` list, which would select the built-in colors.
- An invalid palette color fails with `sRGB24read: no parse`, without naming the key.

### Step 9: Wrap-up

- **Guide:** *Hacking on Decker* is written, Sage is documented and ThebeLab is removed.
  The guide is published at
  `https://decker-edu.github.io/decker/users-guide/decker-users-guide.html`, and `--help`
  links there in place of the old wiki link.
- **Agent docs:** the pointers and the readme use the published URL.
- **Tests:** the numbers and lists are gone, the tests now require zero and empty. Internal
  links of the guide must lead to a heading, with the identifiers of Pandoc.
- **Drafts:** deleted, except `doc/decker-yaml-config-jmu-hci-drp.md`. That pack is not part
  of this repository, so the guide does not replace it.

**Publishing.** The URL was already live, through Jekyll in
`.github/workflows/jekyll-gh-pages.yml`. Jekyll's kramdown does not accept fences with
attributes (`` ``` yaml {.decker-example} ``), so it turned example code into real headings
and broke links. The workflow now builds the guide with the `makefile` and Pandoc, which also
gives a table of contents, and removes the source so that Jekyll only copies the HTML. The URL
stays, the new page appears with the next push to `master`. The workflow step did not run yet:
only the Pandoc command was run, with Pandoc 3.11, while the runner gets the version of Ubuntu.
Links to command headings now use Pandoc's anchors (`#decker-publish`), so they do not work
when reading the file on GitHub.

Checked in Chrome: a `.sage` cell in a `mario` deck, evaluated on `sagecell.sagemath.org`. In
`tudo` the cell only loaded and was not evaluated. The languages `python`, `rr` and `octave`
are from the code.

**Removed from the guide, because it cannot be shown working:** ThebeLab. The library was
removed in 2021 (`8fad3d69`), and no template of any pack loads
`support/plugins/thebelab/thebelab.js`, so `data-executable` blocks stay plain code. The plugin,
`test/decks/thebe-lab-*-deck.md` and the section in `skeleton.yaml` can go.

The deleted drafts also named Pandoc's template slots (`title-prefix`, `pagetitle`,
`author-meta`, `date-meta`, `table-of-contents`, `reference-location`, `idprefix`,
`highlighting-css`) and the keys of the removed `poll` plugin. The guide does not list them.
`attic/MediaTests.hs` still names `doc/media-filter-report-page.md` as its output, but is not
built.
