---
# highlighting: pygments
# highlightjs: solarized-dark
menu: false
title: Color Palettes
---

# Color Palette {.columns}

## 16 base colors {.left}

| Shades                                              | Accents                                             |
|-----------------------------------------------------|-----------------------------------------------------|
| [`shade0`]{style="background-color:var(--shade0);"} | [`accent0`]{style="background-color:var(--accent0);"} |
| [`shade1`]{style="background-color:var(--shade1);"} | [`accent1`]{style="background-color:var(--accent1);"} |
| [`shade2`]{style="background-color:var(--shade2);"} | [`accent2`]{style="background-color:var(--accent2);"} |
| [`shade3`]{style="background-color:var(--shade3);"} | [`accent3`]{style="background-color:var(--accent3);"} |
| [`shade4`]{style="background-color:var(--shade4);"} | [`accent4`]{style="background-color:var(--accent4);"} |
| [`shade5`]{style="background-color:var(--shade5);"} | [`accent5`]{style="background-color:var(--accent5);"} |
| [`shade6`]{style="background-color:var(--shade6);"} | [`accent6`]{style="background-color:var(--accent6);"} |
| [`shade7`]{style="background-color:var(--shade7);"} | [`accent7`]{style="background-color:var(--accent7);"} |

## Specification {.right grow="2"}

``` yaml
palette:
  contrast: 0.35
  colors:
  - "#fdf6e3"
  - "#eee8d5"
  - "#93a1a1"
```

## Derived colors

-   Mix accents against `base00` and `base07`
-   Interpolate in linear RGB (`contrast`)

#  {align="center"}

| `-ffg`                                                | `-fg`                                                | original                                          | `-bg`                                                | `-bbg`                                                |
|-------------------------------------------------------|------------------------------------------------------|---------------------------------------------------|------------------------------------------------------|-------------------------------------------------------|
| [base00]{style="background-color:var(--base00-ffg);"} | [base00]{style="background-color:var(--base00-fg);"} | [base00]{style="background-color:var(--base00);"} | []{style="background-color:var(--base00-bg);"}       | []{style="background-color:var(--base00-bbg);"}       |
| []{style="background-color:var(--base07-ffg);"}       | []{style="background-color:var(--base07-fg);"}       | [base07]{style="background-color:var(--base07);"} | [base07]{style="background-color:var(--base07-bg);"} | [base07]{style="background-color:var(--base07-bbg);"} |
| [base08]{style="background-color:var(--base08-ffg);"} | [base08]{style="background-color:var(--base08-fg);"} | [base08]{style="background-color:var(--base08);"} | [base08]{style="background-color:var(--base08-bg);"} | [base08]{style="background-color:var(--base08-bbg);"} |
| [base09]{style="background-color:var(--base09-ffg);"} | [base09]{style="background-color:var(--base09-fg);"} | [base09]{style="background-color:var(--base09);"} | [base09]{style="background-color:var(--base09-bg);"} | [base09]{style="background-color:var(--base09-bbg);"} |
| [base0A]{style="background-color:var(--base0A-ffg);"} | [base0A]{style="background-color:var(--base0A-fg);"} | [base0A]{style="background-color:var(--base0A);"} | [base0A]{style="background-color:var(--base0A-bg);"} | [base0A]{style="background-color:var(--base0A-bbg);"} |
| [base0B]{style="background-color:var(--base0B-ffg);"} | [base0B]{style="background-color:var(--base0B-fg);"} | [base0B]{style="background-color:var(--base0B);"} | [base0B]{style="background-color:var(--base0B-bg);"} | [base0B]{style="background-color:var(--base0B-bbg);"} |
| [base0C]{style="background-color:var(--base0C-ffg);"} | [base0C]{style="background-color:var(--base0C-fg);"} | [base0C]{style="background-color:var(--base0C);"} | [base0C]{style="background-color:var(--base0C-bg);"} | [base0C]{style="background-color:var(--base0C-bbg);"} |
| [base0D]{style="background-color:var(--base0D-ffg);"} | [base0D]{style="background-color:var(--base0D-fg);"} | [base0D]{style="background-color:var(--base0D);"} | [base0D]{style="background-color:var(--base0D-bg);"} | [base0D]{style="background-color:var(--base0D-bbg);"} |
| [base0E]{style="background-color:var(--base0E-ffg);"} | [base0E]{style="background-color:var(--base0E-fg);"} | [base0E]{style="background-color:var(--base0E);"} | [base0E]{style="background-color:var(--base0E-bg);"} | [base0E]{style="background-color:var(--base0E-bbg);"} |
| [base0F]{style="background-color:var(--base0F-ffg);"} | [base0F]{style="background-color:var(--base0F-fg);"} | [base0F]{style="background-color:var(--base0F);"} | [base0F]{style="background-color:var(--base0F-bg);"} | [base0F]{style="background-color:var(--base0F-bbg);"} |

--------------------------------------------------------------------------------

# Semantic Colors

## Default mapping

``` css
  --background-color: var(--base00);
  --foreground-color: var(--base07);
  --header-color: var(--base06);
  --primary-color: var(--base0D);
  --secondary-color: var(--base09);
  --icon-active-color: var(--base0D);
  --icon-inactive-color: var(--base03);
  --hover-color: var(--base0D-bg);
  --code-background-color: var(--base01);
```

# Blocks {.columns}

## Primary {.accent0 .left}

-   Some text or other
-   $e=mc^2$

## Secondary {.accent1}

-   Some text or other
-   $e=mc^2$

## Success {.accent2}

-   Some text or other
-   $e=mc^2$

## Danger {.accent3 .right}

-   Some text or other
-   $e=mc^2$

## Warning {.accent4 align="center"}

Some text or other

$e=mc^2$

## Info {.accent5 .incremental}

-   Some text or other
-   $e=mc^2$

--------------------------------------------------------------------------------

# Typography {.columns}

## Links {.left}

-   This is a [**link**](https://www.heise.de)
-   This is *importantg*
-   This is **more important**
-   This is [**critically important**]{.accent1}
-   This is *successful*
-   This is **more successful**
-   This is [**critically successful**]{.accent3}
-   `Show me some code`, baby

## Math {.right .accent5 align="right"}

Math blocks are always right!

$$ {e=mc^2} $$

## Surely, you are joking {.accent6 .fragment align="center"}

[**Mr. Feynman**]{.accent7}

--------------------------------------------------------------------------------

# CSS Variables

## Meta data

``` yaml
css-variables:
  base00: white
  base08: red
```

## Translates to

``` html
<style>
  --base00: white;
  --base08: red;
</style>
```

# CSS Variable Defaults

## From `decker.yaml` or `default.yaml`

``` {.yaml .data-line-numbers}
css-variables:
  base00: white
  base08: red
```

## From `css/variables.css`

``` css
:root {
  --icon-size: 2vmin;
  --vertical-margin: 0.1em;
  --list-indent: 1.5em;
  ...
```

--------------------------------------------------------------------------------

![](data/replacement-transform-1-quest.yaml){.question}
