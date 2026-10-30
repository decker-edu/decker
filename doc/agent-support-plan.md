# Agent support: implementation plan

Status: implemented (2026-10-30), see "Implementation notes" at the end.

## Goal

Make Decker easy and safe for AI agents to work with, without copying
version-specific documentation into every project:

1. The binary serves the agent guide at runtime. Committed files are small,
   stable pointers.
2. Decker reports silent content problems itself (`--strict`, machine-readable
   diagnostics) instead of agents grepping HTML.
3. Agents can query project facts (`decker info --json`) instead of guessing.
4. The guide stays short, is split into topics, and its examples are tested.

The order below is the implementation order. Each step is one PR.

--------------------------------------------------------------------------------

## Step 0 — Prerequisite fixes

Small bugs found while testing the current guide.

**0.1 New quest files do not invalidate `questions.json`.**
The rule at `app/Decker.hs:344` reads question sources via `getDeps`, but
`readTargetsFile` deliberately does not `need` the targets file
(`Project.hs:116`), so adding a `*-quest.yaml` leaves the catalog stale until
`-B`.

- Add a Shake oracle returning the sorted list of question sources (follow the
  pattern of `addTargetInfoOracles`). The `questions.json` rule calls
  `askOracle`, so a changed file *set* reruns it. Do the same for the other
  rules that aggregate `deps ^. questions` (`quest-catalog.html`,
  `quest-catalog.xml`).
- Test: build catalog, add a quest file, build again without `-B`, catalog
  contains the new question.
- Afterwards remove the "`-B` is required" advice from guide and skill.

**0.2 `DEVELOPMENT RUN` goes to stdout** (`Shake.hs:73`). That breaks every
machine-readable stdout output (`agent-docs --stdout` today, `info --json` and
`guide` later). Send it to stderr.

--------------------------------------------------------------------------------

## Step 1 — Diagnostics and `--strict`

The biggest lever: turn "exit 0 but wrong" into reported, countable problems.

### 1.1 Diagnostic infrastructure

New module `Text.Decker.Internal.Diagnostics`:

```haskell
data Severity = DiagWarning | DiagError
data Diagnostic = Diagnostic
  { diagSeverity :: Severity,
    diagCode :: Text,          -- stable kebab-case id, e.g. "citation-not-found"
    diagSource :: FilePath,    -- source file (after include resolution: the included file)
    diagLine :: Maybe Int,
    diagMessage :: Text
  }
report :: Diagnostic -> Action ()
```

- `report` prints the familiar human form via `putWarn`
  (`[WARNING] unit/07-deck.md: citation NoSuchKey not found [citation-not-found]`)
  and appends to a per-target collector.
- Collector: a `TVar (Map FilePath [Diagnostic])` in `ActionContext`, keyed by
  the target being built. The deck/page/handout/quest rules clear their entry at
  the start and inspect it at the end (`finishTarget`).
- The `Filter` and `Decker` monads get a lifted `report` so filters can call it.

### 1.2 `--strict`

- New `StrictFlag` (`--strict`) in `deckerFlags` (`Shake.hs`). Also honour a
  meta key `strict: true` so a project can opt in via `decker.yaml`
  (`-m strict=true` then works for free through `MetaValueFlag`).
- `finishTarget`: if strict and the target has any diagnostics, throw a
  `DeckerException` listing them. The HTML has already been written (so it can
  be inspected), but Shake records the rule as failed, so the target is rebuilt
  — and the diagnostics re-reported — on the next run. Exit code is 1.
- Without `--strict`, an up-to-date target is not rebuilt and its warnings are
  not shown again. Persist each target's diagnostics to
  `<transient>/diagnostics/<target>.json` and replay them for wanted targets
  that Shake skipped. (Can follow in a later PR; strict mode does not need it.)

### 1.3 Machine-readable output

- `--diagnostics-json`: in addition to the human lines, emit one JSON object per
  diagnostic on stderr, prefixed with a fixed marker (`decker-diagnostic: {...}`)
  so it can be separated from Shake's own output with a plain `grep`.
- Open decision: alternatively write all diagnostics of a run to one file
  (`--diagnostics=FILE`). Marker lines are simpler for agents; a file is
  cleaner for CI. Recommendation: marker lines first.

### 1.4 Diagnostic sources

| Code | Where | Detection |
|------|-------|-----------|
| `pandoc-*` (e.g. `pandoc-citeproc-warning`) | `processCites` (`Reader/Markdown.hs:72`), `parseMarkdown` | Replace `runIOorExplode` with `runIO` + `setVerbosity ERROR` + `getLog`; convert `LogMessage`s at `WARNING` to diagnostics. The code comes from the constructor (`toJSON` of `LogMessage` carries a `type` field); the text from `showLogMessage`. |
| `citation-without-bibliography` | `processCites`, `otherwise` branch | Walk for `Cite` when no `bibliography` is set. Today this is completely silent. |
| `fragment-templates-disabled` | `expandFragmentTemplates`, else branch (`FragmentTemplate.hs:39`) | Walk for links whose text starts with `§` and code blocks with a class ending in `-`. Do not flag `name-` link texts, which have too many false positives. |
| `quest-unknown-field` | `readQuestion` (`Exam/Question.hs:112`) | Compare the YAML object's keys with the known field set; aeson silently ignores typos such as `ShuffleAnswer`. |
| `setext-heading` (optional) | before `parseMarkdown` | Text lint: a non-blank line directly followed by `^-{3,}\s*$`, outside fenced code and front matter. Heuristic; a warning only. Open decision whether to include it. |

Hard errors (missing include, missing media, unknown fragment template) already
fail the build. Leave them alone, but give the common ones a diagnostic code in
their messages so the guide can reference them.

### 1.5 Tests

Fixture projects under `test/fixtures/diagnostics/` with one deliberate problem
each. Run the pipeline and assert the reported codes. Strict mode: assert that
the target rule fails.

--------------------------------------------------------------------------------

## Step 2 — `decker info --json`

### Interface

`decker info --json` prints one JSON document to stdout and nothing else.
`decker info` without the flag stays the existing human-readable Shake phony.

Dispatch: in `runDeckerArgs` (`Shake.hs:90`) handle `["info"]` together with a
new `JsonFlag` as a command, running in plain IO without Shake. That avoids
Shake's `Build completed` noise on stdout.

### Content (`"schema": 1`)

```json
{
  "schema": 1,
  "decker": { "version": "0.15.0", "branch": "develop", "development": true },
  "directories": { "project": "...", "public": "public", "private": "private", "support": "public/support" },
  "resourcePack": { "configured": "exe:mono", "resolved": "..." },
  "config": {
    "lang": "en",
    "bibliography": "/bibliography/refs.bib",
    "csl": null,
    "strict": false,
    "experiments": { "fragment-templates": true },
    "excludeDirectories": ["code"],
    "staticResourceDirs": ["static"]
  },
  "targets": [
    { "source": "unit/07-deck.md", "kind": "deck",
      "outputs": ["public/unit/07-deck.html", "public/unit/07-handout.html"] },
    { "source": "q/x-quest.yaml", "kind": "quest",
      "outputs": ["private/q/x-quest.html"] }
  ],
  "aggregates": { "examCatalog": "public/exam-builder/questions.json" },
  "fragmentTemplates": [
    { "name": "wen", "origin": "pack" },
    { "name": "wikipedia", "origin": "decker" },
    { "name": "my-box", "origin": "project", "path": "templates/my-box.html" }
  ]
}
```

- `targets`: from `scanTargets meta` (IO), with the `Targets` maps inverted to
  source → outputs.
- `config`: a **curated** subset of the global meta. Do not dump the whole meta:
  it can hold chatty and publishing settings that do not belong in agent
  context.
- `fragmentTemplates`: `fileList` (`Resource.hs:192`) on `support/templates`
  for pack and Decker, plus a glob of the project `templates/` directory. The
  `decker.base-dir` lookup path is mentioned in the guide, not enumerated.
- Test: golden test against a fixture project (normalising absolute paths).

--------------------------------------------------------------------------------

## Step 3 — Runtime guide and stable pointer files

### 3.1 `decker guide`

- New command `guide` in the command list (`Shake.hs:90`). It prints the
  rendered guide to stdout, pack-aware, with the version stamp (accurate,
  because it is produced at the moment it is read).
- Replaces `agent-docs --stdout`. Remove `AgentStdoutFlag`, `AgentOutputFlag`
  and `NoGuideFlag`.

### 3.2 `decker agent-docs` installs pointers only

Writes files whose content does **not** depend on the Decker version, so
re-running after an upgrade changes nothing and collaborators on different
versions do not fight over them:

- `.claude/skills/decker/SKILL.md`: frontmatter (`name`, `description`) plus
  about 20 lines: run `decker guide` before authoring; run `decker info --json`
  for project facts; build with `decker --strict <target>`; never run the
  blocking commands. No version stamp.
- `AGENTS.md`: the same text, in a section delimited by
  `<!-- decker:begin -->` / `<!-- decker:end -->`. Create the file if missing;
  otherwise replace only the delimited section. `--no-agents-md` skips it
  (alongside the existing `--no-skill`).
- If a stale `.decker/agent-guide.md` exists, delete it and say so.
- Keep the idempotent `writeArtifact` (it now matters more: unchanged content
  means a clean git tree).
- Both pointer templates stay overridable through the resource pack.

### 3.3 Template changes

- `skill.md`: rewrite as the short pointer; drop `$decker-version-stamp$`.
- New `agents-md.md`: the section body for `AGENTS.md`.
- `agent-guide.md`: drop the "Location" paragraph. Replace the grep-HTML
  instructions with `--strict` / `--diagnostics-json` and the diagnostic codes
  from step 1; keep "inspect the HTML" only for layout questions.

--------------------------------------------------------------------------------

## Step 4 — Topic guide, tested examples, users' guide

### 4.1 Topics

A single 400-line guide wastes agent context. Split it:

```
resource/decker/template/agent-guide/
  index.md       # loop, blocking commands, topic list (what `decker guide` prints)
  decks.md
  pages.md
  quests.md
  config.md
  templates.md   # fragment templates
```

- `decker guide` prints `index.md`; `decker guide <topic>` prints one topic;
  `decker guide --all` prints everything.
- Packs add topics by shipping `template/agent-guide/<topic>.md`. The index
  lists the union of Decker and pack topics (via `fileList`). A pack topic with
  the same name as a Decker topic replaces it. This replaces the
  `agent-guide-pack.md` partial introduced in the content fix; move mono's file
  to `resource/mono/template/agent-guide/mono.md`.
- `decker info --json` gains a `guideTopics` field.

### 4.2 Tested examples

Doc drift is the main risk; the content fix found five wrong claims in the
previous guide. Make the examples executable:

- Mark examples in topic files with a fence info string:
  ```` ```markdown {.decker-example file="x-deck.md"} ````, and gotcha
  examples with `expect="setext-heading"` (an expected diagnostic code) or
  `expect="error"`.
- A test (`test/AgentGuideTests.hs`) extracts the examples from the templates,
  writes each into a temporary project, builds it with `--strict
  --diagnostics-json`, and checks: positive examples build cleanly, gotcha
  examples produce exactly the expected code. Building in tests needs the Shake
  rules; if that is too heavy for `stack test`, run it as a separate
  `make test-agent-guide` target that uses the built binary, and call it in CI.
- `expect=` codes double as a check that every documented diagnostic code
  exists.

### 4.3 Users' guide

`users-guide/decker-users-guide.md` stays the full human reference; the agent
guide stays terse and verification-oriented. They are not generated from each
other, since the audiences differ too much. Instead:

- Port what the content fix established and the users' guide lacks: `[:include]`
  semantics, the fragment template lookup order and arguments, quest answer
  types, the setext gotcha.
- Every agent topic ends with a pointer to the matching users' guide section.
  Once the users' guide has a stable published URL, link it; until then, name
  the section.

--------------------------------------------------------------------------------

## Open decisions

1. Diagnostics output: marker lines on stderr (recommended) or a file.

marker lines

2. Include the heuristic `setext-heading` lint in strict mode, or report it
   only as a hint.

hint

3. `agent-docs` writes `AGENTS.md` by default (recommended) or only with
   `--agents-md`.

yes


4. Command names: `guide`, `info --json`. `guide` is generic; `agent-guide` is
   more explicit but longer.

agent-guide

## Out of scope

An MCP server. With `--strict`, `--diagnostics-json`, `info --json` and
`guide <topic>`, a CLI covers the same ground; revisit if agents turn out to
need long-running state (e.g. a warm Shake database).

## Implementation notes

All steps are implemented. Where the code differs from the text above:

- **Command names.** Decision 4 chose `agent-guide`, so the guide is printed by
  `decker agent-guide [TOPIC] [--all]`. `decker info --json` uses a new
  `--json` flag.
- **Strict mode** does not throw inside the target rule. Each target's
  diagnostics are persisted (`<transient>/diagnostics/<target>.json`, written
  by `withDiagnostics`), and after the run `finishDiagnostics` (`Shake.hs`)
  replays the persisted diagnostics of live targets that Shake skipped
  (`shakeLiveFiles`). Strict mode then fails the run if the fresh plus replayed
  diagnostics contain any warning. All problems are reported in one run, and
  an up-to-date target still fails. Strict mode is off while watching or
  serving.
- **Targets** are passed to diagnostics through the meta value
  `decker.target`, which `markdownToHtml` sets. Diagnostics without a target
  come from auxiliary reads (index info, chatty) and are dropped, since the
  source's own target reports them.
- **Severities:** `hint` (never fails strict; used for `setext-heading`, per
  decision 2), `warning`, `error`.
- **Codes with line numbers:** pandoc warnings and `setext-heading`. Citation
  and fragment-template diagnostics name the built document, without a line.
- **Hard-error codes:** only `fragment-template-not-found` got a code. Missing
  includes and media are still Shake's `file does not exist` error.
- **Tests** run in `stack test`: `test/DiagnosticsTests.hs` (one fixture
  project per code under `test/fixtures/diagnostics/`, strict, replay, the
  quest catalog oracle, the `info --json` golden file) and
  `test/AgentGuideTests.hs` (the `{.decker-example}` blocks of all topics,
  including pack topics). They run the decker executable that stack puts on
  the PATH, so no separate make target is needed; CI already runs
  `stack test`.
- **Topic files are plain Markdown**, not doctemplates. The index lists the
  topics at the marker `<!-- decker:topics -->`; the version stamp is printed
  as a comment line above the guide.

