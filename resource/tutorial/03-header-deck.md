---
title: YAML-Header options
history: true
---

# Introduction

This deck shows the available options which can be included in the `YAML` header in the `*.md` file.

For further options which might not be included here see: [https://github.com/hakimel/reveal.js/#configuration](https://github.com/hakimel/reveal.js/#configuration)


# Meta files

- The meta configuration options shown here can also be included in any `*-meta.yaml` file located in the top level of the project directory. These options are then active per default for every slide deck but can be overwritten in a specific slide deck.

- For example: The `date` and `menu: true` are added to the `tutorial-meta.yaml` file. If the header of a specific presentation deck includes `menu: false` the menu is not shown in this presentation.


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

# YAML-Header (Part 2)

| Parameter     | Options                          | Effect                         |
|---------------|----------------------------------|--------------------------------|
| `width`, `height`       | numeric                | Define aspect ratio            |
| `menu`        | `true` or `false`                | Include menu showing table of contents |
| `print`       | `true` or `false`                | Show a print button on the title slide |

# YAML-Header (Part 3)

| Parameter     | Options                          | Effect                         |
|---------------|----------------------------------|--------------------------------|
| `progress`    | `true` or `false`                | Turn progress bar on/off       |
| `slideNumber` | `true` or `false`                | Turn slide numbers on/off      |
| `history`     | `true` or `false`                | Show slides in browser history |
| `controls`    | `true` or `false`                | Turn arrow controls on/off     |

# YAML-Header (Part 4)

| Parameter     | Options                          | Effect                         |
|---------------|----------------------------------|--------------------------------|
| `csl`         | Filepath to .csl file            | Include a citation style (.csl)|
| `bibliography`| Filepath to .bib file            | Include bibliography           |
| `chalkboard`  | `true` or `false`                | Include reveal.js chalkboard plugin |
| `dachdecker`  | Code/id given by dachdecker      | Include a dachdecker survey    |
| `chart`       | `true` or `false`                | Include reveal.js chart plugin |

# YAML-Header (Part 5)


| Parameter     | Options                          | Effect                         |
|---------------|----------------------------------|--------------------------------|
| `lang`        | Any ISO Language Code (eg. `de`) | HTML content language          |
| `css`         |                                  | Additional CSS resources       |
| `dir`         | `RTL` or `LTR`                   | Text content direction         |
