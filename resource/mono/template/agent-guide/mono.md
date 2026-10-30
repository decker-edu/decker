# Resource pack `mono`

Applies when the project uses the `mono` resource pack
(`resource-pack: exe:mono`; check `resourcePack` in `decker info --json`). The
pack enables `experiments.fragment-templates` and `page-features.codapi` by
default, so fragment templates and live code work without further
configuration. Slides are 1920 by 1080.

```yaml {.decker-example file="decker.yaml" project="mono"}
resource-pack: exe:mono
```

## Differences from the defaults

- **Quizzes do not react**: `decker-plugins.quiz-wue` is off, as are `zoom`, `jingles`,
  `feedback`, `notes` and `quiz`. `chatty` and `quizzer` are on. Turn on what the deck
  needs:

  ```yaml {.decker-example file="decker.yaml" project="quiz"}
  resource-pack: exe:mono
  decker-plugins:
    quiz-wue: true
  ```

  ```markdown {.decker-example file="quiz-deck.md" project="quiz"}
  # Question

  ## {.qmc}

  - [X] Right
  - [ ] Wrong
  ```

- **Source links**: `experiments.add-document-path` adds a link to the source file to
  each level 1 heading, with the absolute path of the build machine. Plain `decker
  publish` publishes it, so set the key to `false` for published material.
- **Live quizzes** need `quizzer.url` in the project, the pack only sets `poll-server`.
- **Accent order**: `.accent0` is magenta and `.accent1` red, one off from other packs.
  `--redish` and the other named variables are still correct.

## Additions to the styles

```markdown {.decker-example file="styles-deck.md" project="mono"}
# Styles

[badge]{.accent1} [text color only]{.accent1 .fg} [red text]{.red} [filled]{.red .bg}

[not published yet](https://example.com){.off}
```

`document-spacing` in `css-variables` scales the vertical spacing of pages and the
index.

## Additional fragment templates

| Template                             | Invocation                              | Result                          |
|--------------------------------------|-----------------------------------------|---------------------------------|
| `wen`                                | `[§wen](Article_Name "Display Text")`   | English Wikipedia link          |
| `wde`                                | `[§wde](Artikel_Name "Anzeigetext")`    | German Wikipedia link           |
| `wp`                                 | `[§wp en](Article_Name "Display Text")` | Wikipedia link, language `arg1` (default `de`) |
| `wp-cc`                              | `[§wp-cc sa 4.0](File_Name.png)`        | Wikimedia file + CC BY licence note |
| `mdn`                                | `[§mdn](CSS/display "display")`         | MDN Web Docs link (German)      |
| `unit`, `subunit`, `assign`          | `[§unit](07 "Title")`                   | Index links to `unit/<url>-deck.html`, `unit/<url>/<url>-deck.html`, `assignments/<url>-page.html` |
| `unit-nl`, `subunit-nl`, `assign-nl` | `[§unit-nl](07 "Title")`                | Same entries without a link (not yet published) |
| `nop`                                | `[§nop](x)`                             | Renders nothing                 |

```markdown {.decker-example file="links-deck.md" project="mono"}
# Links

- [§wen](Haskell_(programming_language) "Haskell")
- [§wde](Rekursion "Rekursion")
- [§mdn](CSS/display "display")
```

## Live code (Codapi)

Declare the Codapi server once per deck or page. Runnable snippets use the
`live-code` template. **The trailing dash in `.live-code-` is intentional** —
never "fix" it to `.live-code`:

````markdown {.decker-example file="live-deck.md" project="mono"}
# Live code

[§live-code-server](https://example.com/codapi/v1)

```{#base .live-code- language="Haskell" sandbox="GHCi"}
x = 42
```

```{.live-code- language="Haskell" depends-on="base"}
main = print x
```
````

- `depends-on="<id>"` runs the code of another snippet first.
- `sandbox` names the Codapi sandbox (default: the language); `engine`
  defaults to `codapi`. Other attributes are passed through to
  `<codapi-snippet>`.

See the [users' guide](https://decker-edu.github.io/decker/users-guide/decker-users-guide.html), sections *Live code* and *mono*.
