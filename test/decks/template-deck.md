---
lork: true
templates:
  block-test: |
    :(1) :(2) :(3) :(param1) :(param2) :(code)
  test: ":(1) :(2) :(3) [:(title)](:(url)) :(args)"
  test2: |
    <a href=":(url)">Pony</a> :(args) <a href=":(url)">Unicorn</a>
  video: |
    <video controls style="width: var(--slide-width); height: var(--slide-height);">
    <source src=":(url)" type="video/mp4" />
    <track kind="subtitles" label="Deutsch" srclang="de" src=":(title)" default />
    </video>
title: Template Macros
meta-data:
  - /test/decks/macro-templates.yaml
---

# Template

## Macro definition

``` yaml
templates:
  test: :(1) :(2) :(3) [:(title)](:(url))
```

## Macro invocation

``` markdown
[@test arg1 arg2 arg3](the%20url "Title")
```

## Result

Hallo [@test arg1 arg2 arg3](the%20url "Title") Was denn [@test2 There is
no](the%20url "Title")

--------------------------------------------------------------------------------

# Wikipedia

## Macro definition

``` yaml
  wp: |
    <a href=":(url)"><i class="fa-brands fa-wikipedia-w"></i> - :(title)</a>
```

## Invocation

``` markdown
[@wp](https://de.wikipedia.org/wiki/SRGB-Farbraum "sRGB Farbraum")
```

## Result

[@wp](https://de.wikipedia.org/wiki/SRGB-Farbraum "sRGB Farbraum")

--------------------------------------------------------------------------------

# Videos with Subtitles

## The data definition

``` yaml
templates:
  video: |
    <video controls style="width: var(--slide-width); height: var(--slide-height);">
    <source src=":(url)" type="video/mp4" />
    <track kind="subtitles" label="Deutsch" srclang="de" src=":(title)" default />
    </video>
```

## Translates the link tag

``` markdown
[@video](/videos/myvideo.mp4 "/videos/mycaptions.vtt")
```

--------------------------------------------------------------------------------

# Into

``` html
<video controls style="width: var(--slide-width); height: var(--slide-height);">
<source src="/videos/myvideo.mp4" type="video/mp4" />
<track kind="subtitles" label="Deutsch" srclang="de" src="/videos/mycaptions.vtt" default />
</video>
```

# Codeblock Macro

``` {.arg1 .arg2 .arg3 macro="block-test" param1="p1" param2="p2"}
The Block Test Code
```
