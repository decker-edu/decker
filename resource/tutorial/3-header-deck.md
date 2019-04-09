---
title: YAML-Header options
history: true
---

# Introduction

This deck shows the available options which can be included in the `YAML` header in the `*.md` file.

For further options which might not be included here see: [https://github.com/hakimel/reveal.js/#configuration](https://github.com/hakimel/reveal.js/#configuration)

# Example Header

This header is located directly at the top of the `*.md` file.

```yaml
---
title: Decker Slide Tool Reference Guide
history: true
menu: true
bibliography: example.bib
csl: chicago-author-date.csl
controls: true
chalkboard: example-deck.json
---
```

# YAML-Header (Part 1)

| Parameter     | Options                          | Effect                         |
|---------------|----------------------------------|--------------------------------|
| `author`      | String                           | Displayed on first slide       |
| `date`        | String                           | Displayed on first slide       |
| `title`       | String                           | Displayed on first slide       |
| `subtitle`    | String                           | Displayed on first slide       |
| `controls`    | `0` or `1`                       | Turn arrow controls on/off     |
| `width`, `height`       | numeric                | Define aspect ratio            |

# YAML-Header (Part 2)

| Parameter     | Options                          | Effect                         |
|---------------|----------------------------------|--------------------------------|
| `menu`        | `true` or `false`                | Include menu showing table of contents |
| `progress`    | `0` or `1`                       | Turn progress bar on/off       |
| `slideNumber` | `true` or `false`                | Turn slide numbers on/off      |
| `lang`        | Any ISO Language Code (eg. `de`) | HTML content language          |
| `history`     | `true` or `false`                | Show slides in browser history (remember current slide e.g. on reload) |

# YAML-Header (Part 3)


| Parameter     | Options                          | Effect                         |
|---------------|----------------------------------|--------------------------------|
| `csl`         | Filepath to .csl file            | Include a citation style (.csl)|
| `bibliography`| Filepath to .bib file            | Include bibliography           |
| `chalkboard`  | `true` or `false`                | Include reveal.js chalkboard plugin |
| `dachdecker`  | Code given by dachdecker         | Include a dachdecker survey    |
| `chart`       | `true` or `false`                | Include reveal.js chart plugin |

# YAML-Header (Part 4)


| Parameter     | Options                          | Effect                         |
|---------------|----------------------------------|--------------------------------|
| `keywords`    |                                  | HTML document header meta data |
| `theme`       |                                  |                                |
| `css`         |                                  | Additional CSS resources       |
| `author-meta` |                                  | HTML document header meta data |
| `date-meta`   |                                  | HTML document header meta data |
| `dir`         | `RTL` or `LTR`                   | Text content direction         |
