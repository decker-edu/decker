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

-   Mix accents against `shade0` and `shade7`
-   Interpolate in linear RGB (`contrast`)

#  {align="center"}

| `-ffg`                                                | `-fg`                                                | original                                          | `-bg`                                                | `-bbg`                                                |
|-------------------------------------------------------|------------------------------------------------------|---------------------------------------------------|------------------------------------------------------|-------------------------------------------------------|
| [shade0]{style="background-color:var(--shade0-ffg);"} | [shade0]{style="background-color:var(--shade0-fg);"} | [shade0]{style="background-color:var(--shade0);"} | []{style="background-color:var(--shade0-bg);"}       | []{style="background-color:var(--shade0-bbg);"}       |
| []{style="background-color:var(--shade7-ffg);"}       | []{style="background-color:var(--shade7-fg);"}       | [shade7]{style="background-color:var(--shade7);"} | [shade7]{style="background-color:var(--shade7-bg);"} | [shade7]{style="background-color:var(--shade7-bbg);"} |
| [accent0]{style="background-color:var(--accent0-ffg);"} | [accent0]{style="background-color:var(--accent0-fg);"} | [accent0]{style="background-color:var(--accent0);"} | [accent0]{style="background-color:var(--accent0-bg);"} | [accent0]{style="background-color:var(--accent0-bbg);"} |
| [accent1]{style="background-color:var(--accent1-ffg);"} | [accent1]{style="background-color:var(--accent1-fg);"} | [accent1]{style="background-color:var(--accent1);"} | [accent1]{style="background-color:var(--accent1-bg);"} | [accent1]{style="background-color:var(--accent1-bbg);"} |
| [accent2]{style="background-color:var(--accent2-ffg);"} | [accent2]{style="background-color:var(--accent2-fg);"} | [accent2]{style="background-color:var(--accent2);"} | [accent2]{style="background-color:var(--accent2-bg);"} | [accent2]{style="background-color:var(--accent2-bbg);"} |
| [accent3]{style="background-color:var(--accent3-ffg);"} | [accent3]{style="background-color:var(--accent3-fg);"} | [accent3]{style="background-color:var(--accent3);"} | [accent3]{style="background-color:var(--accent3-bg);"} | [accent3]{style="background-color:var(--accent3-bbg);"} |
| [accent4]{style="background-color:var(--accent4-ffg);"} | [accent4]{style="background-color:var(--accent4-fg);"} | [accent4]{style="background-color:var(--accent4);"} | [accent4]{style="background-color:var(--accent4-bg);"} | [accent4]{style="background-color:var(--accent4-bbg);"} |
| [accent5]{style="background-color:var(--accent5-ffg);"} | [accent5]{style="background-color:var(--accent5-fg);"} | [accent5]{style="background-color:var(--accent5);"} | [accent5]{style="background-color:var(--accent5-bg);"} | [accent5]{style="background-color:var(--accent5-bbg);"} |
| [accent6]{style="background-color:var(--accent6-ffg);"} | [accent6]{style="background-color:var(--accent6-fg);"} | [accent6]{style="background-color:var(--accent6);"} | [accent6]{style="background-color:var(--accent6-bg);"} | [accent6]{style="background-color:var(--accent6-bbg);"} |
| [accent7]{style="background-color:var(--accent7-ffg);"} | [accent7]{style="background-color:var(--accent7-fg);"} | [accent7]{style="background-color:var(--accent7);"} | [accent7]{style="background-color:var(--accent7-bg);"} | [accent7]{style="background-color:var(--accent7-bbg);"} |

--------------------------------------------------------------------------------

# Semantic Colors

## Default mapping

``` css
  --background-color: var(--shade0);
  --foreground-color: var(--shade7);
  --header-color: var(--shade6);
  --primary-color: var(--accent5);
  --secondary-color: var(--accent1);
  --icon-active-color: var(--accent5);
  --icon-inactive-color: var(--shade3);
  --hover-color: var(--accent5-bg);
  --code-background-color: var(--shade1);
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
  shade0: white
  accent0: red
```

## Translates to

``` html
<style>
  --shade0: white;
  --accent0: red;
</style>
```

# CSS Variable Defaults

## From `decker.yaml` or `default.yaml`

``` {.yaml .data-line-numbers}
css-variables:
  shade0: white
  accent0: red
```

## From `css/variables.css`

``` css
:root {
  --icon-size: 2vmin;
  --vertical-margin: 0.1em;
  --list-indent: 1.5em;
  ...
```
