# Decker agent guide

This guide tells an AI agent how to **author Decker content and prove it is
correct** before reporting done. It is printed by the `decker` binary itself,
so it always matches the installed version. This page covers the build loop;
syntax lives in topics (list at the end), e.g. `decker agent-guide decks`.

Code blocks marked `{.decker-example ...}` are built by Decker's test suite
against this version; examples with `expect="<code>"` show a mistake and the
diagnostic it produces.

A project's `CLAUDE.md` or `AGENTS.md` should hold only **project-specific**
context (conventions, layout, naming, publishing). Generic Decker knowledge
comes from this guide.

## Project facts

Do not guess configuration. Run

```sh
decker info --json
```

It prints one JSON document: every source with its build targets, the
resource pack, the relevant configuration (`lang`, `bibliography`,
`experiments`, `strict`, …), the fragment templates that are available and
the guide topics. Anything in this guide that depends on configuration is
decided by those facts.

## The loop

After writing or editing any `*-deck.md`, `*-page.md` or `*-quest.yaml`, build
the target in strict mode and read the exit code. **Never report done after
only writing the file.**

```sh
decker --strict public/unit/07-deck.html
```

- **Exit 0**: the target built and Decker found no content problems.
- **Exit 1** with `[WARNING] <file>: <message> [<code>]` lines: the target was
  built, but the content is wrong in a way the build cannot fix (see the
  codes below). The HTML is written, so it can be inspected. Fix the source
  and build again.
- **Exit 1** with `Error when running Shake build system`: a hard error, no
  output was produced. Usually `file does not exist and no rule available:
  <path>` (a missing include or media file) or a message ending in
  `[fragment-template-not-found]`.

Warnings of targets that are already up to date are reported again, so an
unchanged target still fails a strict build.

For machine-readable output add `--diagnostics-json`. Each diagnostic is then
also printed on stderr as one line:

```
decker-diagnostic: {"code":"citation-not-found","line":null,"message":"Citeproc: citation Foo not found","severity":"warning","source":"unit/07-deck.md","target":"public/unit/07-deck.html"}
```

Ignore the banner `WARNING: You are running a development build` and the
Shake progress lines (`# pandoc (for ...)`, `Build completed in ...`).

Content problems are reported by the build; layout problems (overflowing
slides, broken columns) are not. Check those visually as described in
`decker agent-guide decks`, section *Layout check*.

## Sources and targets

| Source                    | Build target                             |
|---------------------------|------------------------------------------|
| `unit/07-deck.md`         | `public/unit/07-deck.html`               |
| `unit/07-deck.md`         | `public/unit/07-handout.html` (handout)  |
| `assignments/a01-page.md` | `public/assignments/a01-page.html`       |
| `index.md`                | `public/index.html`                      |
| `topic/q/x-quest.yaml`    | `private/topic/q/x-quest.html` (preview) |
| all `*-quest.yaml`        | `public/exam-builder/questions.json`     |

Markdown files without one of these suffixes (e.g. `topic/intro.md`) produce
no output of their own; they are only used via `[:include](...)`. To validate
one, build a deck or page that includes it. `decker info --json` lists the
targets of every source.

## Diagnostic codes

| Code                            | Problem                                                                    |
|---------------------------------|----------------------------------------------------------------------------|
| `citation-not-found`            | A citation key is not in the bibliography.                                 |
| `citation-without-bibliography` | `[@key]` is used, but no `bibliography` is configured; the key is printed literally. |
| `fragment-templates-disabled`   | `[§name](...)` or a `.name-` code block is used, but `experiments.fragment-templates` is off; it is not expanded. |
| `quest-unknown-field`           | A question file has a field the parser ignores (usually a typo).          |
| `setext-heading`                | *Hint only*: a dash line directly below text makes that text a level-2 heading instead of a slide break. Never fails a build. |
| `pandoc-*`                      | Any other pandoc warning, e.g. `pandoc-duplicate-identifier`.              |

Hard errors fail the build regardless of `--strict`: a missing include or
media file, an unknown fragment template (`fragment-template-not-found`), a
question file that does not parse.

## Commands that block

These never return. Never run them in the foreground:

- `decker serve` — only in the background, as `decker serve -p <free port>`,
  for a layout check (`decker agent-guide decks`). Stop it afterwards.
- `decker --server` / `-S`, `decker --watch` / `-w` — never; they rebuild on
  every file change, while you are still editing.
- `decker exam-builder` — never; it opens a browser. To build its data, build
  `public/exam-builder/questions.json`.

## Commands with effects outside the project

Run these only when the user asks for exactly that:

- `decker publish` copies `public/` to the configured destination, drafts
  included, and deletes what else is there. `decker publish -l` also removes
  `public/` and `private/` first and syncs the Chatty vector store.
- `decker chatty` syncs the project to the Chatty vector store. With
  `--prune-files` it deletes files in the OpenAI project.

`decker clean` removes `public/`, `private/` and `chatty/`, and `decker purge`
also the build caches. Both are safe, but the next build takes longer.

## Command reference

| Purpose                              | Command                                          |
|--------------------------------------|--------------------------------------------------|
| Build and check one target           | `decker --strict public/<path>.html`             |
| Preview one question                 | `decker --strict private/<path>-quest.html`      |
| Build the exam question catalog      | `decker --strict public/exam-builder/questions.json` |
| Build all decks and pages            | `decker --strict html`                           |
| Build one kind of document           | `decker --strict decks` (or `pages`, `handouts`, `questions`) |
| Build Moodle XML                     | `decker --strict moodle-xml`                     |
| Build PDFs of all decks (needs Chrome) | `decker pdf`, on a free port: `decker pdf -p <port>` |
| Serve `public/` for a layout check   | `decker serve -p <free port>` (in the background, builds nothing) |
| Project facts as JSON                | `decker info --json`                             |
| One guide topic / everything         | `decker agent-guide <topic>` / `--all`           |
| Check external tools                 | `decker check`                                   |
| Print version                        | `decker version`                                 |
| Format Markdown (stdin → stdout)     | `decker format`                                  |

A project can make strict mode the default with `strict: true` in
`decker.yaml`.

## Topics

<!-- decker:topics -->
The [users' guide](https://decker-edu.github.io/decker/users-guide/decker-users-guide.html)
is the full human reference; its sections *Targets*, *Commands*, *Options* and
*Agent support* describe all commands. It follows the development branch, so it may be newer than the
installed version. Where it disagrees with this guide, this guide is right.
