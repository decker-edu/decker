---
highlighting: pygments
title: Color Palettes  
menu: false
---

# Color Palette {.columns}

## 16 base colors {.left}

| Shades                                              | Accents                                             |
|-----------------------------------------------------|-----------------------------------------------------|
| [`base00`]{style="background-color:var(--base00);"} | [`base08`]{style="background-color:var(--base08);"} |
| [`base01`]{style="background-color:var(--base01);"} | [`base09`]{style="background-color:var(--base09);"} |
| [`base02`]{style="background-color:var(--base02);"} | [`base0A`]{style="background-color:var(--base0A);"} |
| [`base03`]{style="background-color:var(--base03);"} | [`base0B`]{style="background-color:var(--base0B);"} |
| [`base04`]{style="background-color:var(--base04);"} | [`base0C`]{style="background-color:var(--base0C);"} |
| [`base05`]{style="background-color:var(--base05);"} | [`base0D`]{style="background-color:var(--base0D);"} |
| [`base06`]{style="background-color:var(--base06);"} | [`base0E`]{style="background-color:var(--base0E);"} |
| [`base07`]{style="background-color:var(--base07);"} | [`base0F`]{style="background-color:var(--base0F);"} |

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

: [gruvbox](https://github.com/morhetz/gruvbox) color scheme

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


## Primary {.primary .left}

-   Some text or other
-   $e=mc^2$

## Secondary {.secondary}

-   Some text or other
-   $e=mc^2$

## Success {.success}

-   Some text or other
-   $e=mc^2$

## Danger {.danger .right}

-   Some text or other
-   $e=mc^2$

## Warning {.warning align="center"}

Some text or other

$e=mc^2$

## Info {.info}

-   Some text or other
-   $e=mc^2$

--------------------------------------------------------------------------------

# Typography {.columns}

## Links {.left}

-   This is a [**link**](https://www.heise.de)
-   This is *important*
-   This is **more important**
-   This is [**critically important**]{.warning}
-   This is *successful*
-   This is **more successful**
-   This is [**critically successful**]{.success}

## Math {.right .warning}

Math always is both

$$
{e=mc^2}
$$

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

``` yaml
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
