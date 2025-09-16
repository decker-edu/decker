---
author: Henrik Tramberend
copyright: Copyright 2019
date: 2019-10-19
quiz:
  style: plain
showNotes: true
subtitle: Generate handouts from slide source
title: Handout Test
title-prefix: Decker
---


# Notes

## Currently

-   Content marked `.notes` serves two purposes
-   Reveal.js uses it as *speaker notes*
-   It augments slide content in handouts

## In the future

-   Content marked `.notes` is used only as *speaker notes*
-   Content marked `.handout` is only shown in handout documents
-   Content marked `.deck` is only shown in slide decks
-   Untagged content is shown in decks and handouts

--------------------------------------------------------------------------------

# Untagged content {layout="columns"}

## This is untagged content {.left}

-   The entire content shows up in *slide decks* and in *handouts*

##  {.right}

![](include/06-metal.png)

--------------------------------------------------------------------------------

# Content tagged `handout` {.handout layout="columns"}

## This slide is tagged `handout` {.left}

-   The entire slide only shows up in *handouts*

##  {.right}

![](include/06-metal.png)

--------------------------------------------------------------------------------

# Content tagged `deck` {.deck layout="columns"}

## This slide is tagged `deck` {.left}

-   The entire slide only shows up in *slide decks*

##  {.right}

![](include/06-metal.png)

# Content tagged `deck` and `handout` {layout="columns"}

## This column is tagged `deck` {.deck .left}

-   This column only shows up in *slide decks*

![](include/06-metal.png)

## This column is tagged `handout` {.handout .right}

-   This column only shows up in *handouts*

![](include/06-metal.png)

# Content tagged `notes`

## This slide contains speaker notes

-   The second paragraph contains speaker notes
-   Speaker Notes are not visible in decks and handouts

## Speaker Notes from a block {.notes}

-   This content is included as speaker notes
-   It is not included in handouts

![](include/06-metal.png)

# Slide tagged `notes`

## This slide has some speaker notes

-   The following slide contains the speaker notes
-   Slides with speaker Notes are not visible in decks and handouts

# Speaker notes from an entire slide {.notes}

## This slide is not shown anywhere

-   It shows up as speaker notes to the slide before it

# Div and Span tagged `notes`

## Div

The follwing Div is not visible here

::: notes
This is a speaker note from a Div
:::

## Span

The follwing Span is not visible here: [This is a speaker note from a
Span]{.notes}

# Backgrounds 1 ![In a slide, this is a background image.](include/06-metal.png)

## Background Images in Handouts

-   Converted to inline images
-   Just after the header

# Backgrounds 2 ![In a slide, this is a background video.](pacman-perfect-game.mp4){.inverse controls="1"}

## Background Videos in Handouts

-   Converted to inline videos
-   Just after the header

# Two columns with figures {.columns}

## Left {.left}

![Caption text is here.](include/06-metal.png)

## Right {.right}

![Caption text is here.](include/06-metal.png)

# Links

## Do we really need them readable

-   This can get [ugly](https://en.wikipedia.org/wiki/Unattractiveness) fast

# Just a table {.columns}

## Once {.left .fragment}

| This | is  | just | a   | table |
|------|:----|------|-----|-------|
| This | is  | just | a   | table |
| This | is  | just | a   | table |
| This | is  | just | a   | table |
| This | is  | just | a   | table |

## Twice {.right}

| This |  is | just |   a | table |
|------|----:|:-----|----:|:------|
| This |  is | just |   a | table |
| This |  is | just |   a | table |
| This |  is | just |   a | table |
| This |  is | just |   a | table |

# A slide with a comment code block

## Below should be nothing

``` comment
THIS IS A COMMENT
AWAY WITH IT  
```

## Above should be nothing
