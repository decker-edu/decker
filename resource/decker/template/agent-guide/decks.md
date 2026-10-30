# Slide decks (`*-deck.md`)

A deck is a Markdown file with optional YAML front matter. It builds to
`public/<path>-deck.html` and a handout `public/<path>-handout.html`.

````markdown {.decker-example file="intro-deck.md"}
---
title: Slide Deck Title
lang: en
---

# First Slide

- Bullet point
- Another

--------------------------------------------------------------------------------

# Second Slide

Content.
````

## Slides

- A new slide starts at every **level-1 heading** (`# ...`) and at every
  **horizontal rule** (a line of three or more dashes with blank lines around
  it). A rule directly followed by a heading counts as one slide break.
- The 80-dash rule line is a convention, not a requirement.
- `# Title {.sub}` makes a vertical sub-slide below the previous slide.

**Gotcha:** a dash line directly below a line of text (no blank line between)
is not a rule. It turns that text into a level-2 heading. The build reports a
`setext-heading` hint:

```markdown {.decker-example file="setext-deck.md" expect="setext-heading"}
# Slide

Some text
---

# Next slide
```

Always put a blank line before a separator.

## Slide and block classes

Attributes go in `{...}` after a heading. Level-2 headings inside a slide
start **blocks**; layout classes apply to blocks.

```markdown {.decker-example file="columns-deck.md"}
# Two Columns {.columns}

## Left {.left grow="2"}

Left content.

## Right {.right grow="3"}

Right content.

## Below {.bottom .fragment}

Appears at the bottom after a click.
```

| Class / attribute                                  | Effect                                |
|----------------------------------------------------|---------------------------------------|
| `.columns`                                         | Column layout for the slide's blocks  |
| `.left`, `.right`                                  | Place block in a column               |
| `.top`, `.bottom`                                  | Place block above / below the columns |
| `grow="n"`                                         | Relative column width                 |
| `.fragment`                                        | Reveal on click                       |
| `.incremental`                                     | Reveal list items one by one          |
| `.accent0` … `.accent7`                            | Accent colour                         |
| `.solution`                                        | Mark a block as solution              |
| `popup="center"` (also `center-up`, `center-down`) | Show block as overlay                 |

## Including other files

```markdown {.decker-example file="main-deck.md" project="include"}
# Main

[:include](parts/intro.md)
```

```markdown {.decker-example file="parts/intro.md" project="include"}
---
title: Ignored when included
---

# Included slide

From another file.
```

- `[:include](path)` must be the **only** content of its paragraph.
- Relative paths resolve against the including file; a leading `/` resolves
  against the project root.
- The included file's YAML front matter is ignored; only its body is inserted.
- Includes nest. Parser warnings and hints name the included file; other
  diagnostics name the deck that is built.
- A missing file is a **hard** error:

```markdown {.decker-example file="broken-deck.md" expect="error"}
# Main

[:include](does-not-exist.md)
```

## Code

Fence with a language identifier for highlighting:

````markdown {.decker-example file="code-deck.md"}
# Code

```haskell
factorial n = product [1..n]
```
````

## Math (LaTeX via MathJax)

```markdown {.decker-example file="math-deck.md"}
# Math

Inline: $E = mc^2$

Display:

$$
\bar{x} = \frac{1}{n}\sum_{i=1}^n x_i
$$
```

## Images and media

```markdown
![Caption](image.png){width="50%"}
```

A missing local media file is a **hard** error.

## Citations

Pandoc citation syntax. It needs `bibliography:` in `decker.yaml` or the front
matter (optionally `csl:`).

```yaml {.decker-example file="decker.yaml" project="cite"}
bibliography: refs.bib
```

```bibtex {.decker-example file="refs.bib" project="cite"}
@book{Knuth1984,
  author = {Knuth, Donald E.},
  title = {The TeXbook},
  publisher = {Addison-Wesley},
  year = {1984}
}
```

```markdown {.decker-example file="cite-deck.md" project="cite"}
# Citations

As shown in [@Knuth1984].
```

An unknown key is reported as `citation-not-found`. Without a bibliography,
`[@key]` is reported as `citation-without-bibliography` and printed literally:

```markdown {.decker-example file="nobib-deck.md" expect="citation-without-bibliography"}
# Citations

As shown in [@Knuth1984].
```

Bare in-text citations (`@key` without brackets) are also citations when a
bibliography is configured.

## Layout check

The build cannot see layout. After changing slides with columns, images,
long code blocks, tables or a lot of text, look at the rendered slides.
Decks do not work from `file://` URLs; they must be served.

1. Build the deck: `decker --strict public/unit/07-deck.html`.
2. Start the server **in the background** on a free port above 1024, e.g.
   `decker serve -p 18342`. It serves
   `public/` and does not build, so build again after every change;
   reloading the page is enough. If the port is in use, it exits with status
   7; pick another port. Stop the server when you are done.
3. Open one slide, e.g. `http://127.0.0.1:18342/unit/07-deck.html#/results`,
   and take a screenshot. The slide id is the `id` of its `<section>` in the
   HTML: the heading text in kebab case (`# First Slide` → `first-slide`), an
   explicit id (`# Results {#results}`), `title-slide`, or `empty-<n>` for a
   slide without heading. Use a viewport with the aspect ratio of
   `config.slideSize` from `decker info --json` (default 1280×720); the deck
   scales to the window.

The server listens on `127.0.0.1`. Open that address, not `localhost`: a
`localhost` URL may reach some other program listening on IPv6 `::1`.

Look for:

- content cut off at the bottom or right edge of the slide,
- columns stacked instead of side by side, or squeezed to a sliver,
- images pushing other content off the slide,
- popups or fragments covering other content.

`.fragment` content is hidden until revealed. Step through the fragments
(arrow keys) and check the last state, where most content is shown.

See the [users' guide](https://github.com/decker-edu/decker/blob/develop/users-guide/decker-users-guide.md), section *Decker's Markdown*, for media, layout and
the remaining features.
