# Pages (`*-page.md`)

A page is an ordinary Markdown document with YAML front matter (`title`,
`lang`, …). It builds to `public/<path>-page.html`.

```markdown {.decker-example file="about-page.md"}
---
title: About this course
lang: en
---

# Schedule

| Week | Topic        |
|------|--------------|
| 1    | Introduction |
| 2    | Recursion    |

## Reading

See the [slides](intro-deck.html).
```

Headings, lists, code, images, tables, math, citations and `[:include](...)`
work as in decks (`decker agent-guide decks`). Slide separators and layout
classes do not apply: a page is one continuous document.

`index.md` in the project root is a page, too. It builds to
`public/index.html`. Without it, Decker generates an index of all decks and
pages.

See the [users' guide](https://github.com/decker-edu/decker/blob/develop/users-guide/decker-users-guide.md), section *Decker's Markdown*.
