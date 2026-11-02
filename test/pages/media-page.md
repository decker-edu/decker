---
author: Arthur Author
date: 06.02.2022
highlight-style: tango
institute: Institute
lang: en
subtitle: Clean. Consistent. Robust.
title: Media Handling on a Page
---

This page is the counterpart of `test/decks/media-deck.md`. It uses the same
media elements, but in one continuous document. Slide separators, columns,
fragments and popups do not apply to pages and are left out.

# Images

## Local image

![Alt Caption $e=mc^2$](../decks/include/06-metal.png)

## Local image with caption block

The text of the image is discarded in favor of the block caption.

![DISCARDED](../decks/include/06-metal.png)

Caption: Block Caption $e=mc^2$

## Width

`{width="50%"}`

![Width is 50%, caption $e=mc^2$](../decks/include/06-metal.png){width="50%"}

## Height and alignment

`{align="right" height="200px"}`

![Aligned right, caption $e=mc^2$](../decks/include/06-metal.png){align="right"
height="200px"}

## Width, height and title

`{align="right" width="80%" height="200px"}`

![Both dimensions, caption
$e=mc^2$](../decks/include/06-metal.png "lallalal"){align="right" width="80%"
height="200px"}

# Media in a row

## Two images in a row

Four images, actually. A line block puts media side by side.

| ![Alt Caption $e=mc^2$](../decks/include/06-metal.png)
| ![Tilt!](../decks/include/06-metal-v.png)
| ![Alt Caption $e=mc^2$](../decks/dumbell-hard.svg){w:h="3/2.03"}
| ![Tilt!](../decks/include/06-metal-v.png)

Caption: Aspect ratio of images is determined at compile time, or can be
specified.

## Three somethings in a row

| ![Alt Caption $e=mc^2$](../decks/include/06-metal.png)
| ![Alt Caption $e=mc^2$](../decks/pacman-perfect-game.mp4){.controls}
| ![Even this!](../decks/assets/teapot.off){w:h="1/1"}

Caption: Caption all the things

# Inline media

Some text ![](../decks/pacman-perfect-game.mp4){.controls .autoplay height="3em"}
some more ![](../static/es6.js){.code height="3em" width="10em"} code.

![One](../decks/include/06-metal.png){height="60px"}![Two](../decks/include/06-metal.png){height="120px"}![Three](../decks/include/06-metal.png){height="240px"}

# Embedded SVG

## From a local file

![Why is it called dumbell?](../decks/dumbell-hard.svg){.embed width="400px"}

## With a fixed height

![Caption](../decks/assets/slab-test-0.svg){.embed height="256px"}

# Video

## Local video

![The perfect game.](../decks/pacman-perfect-game.mp4){.controls .autoplay
width="60%"}

## YouTube video stream

![Avatar.](youtube:7XX6IEuLP3A){width="80%"}

# Embedded documents

## Iframe

![Homepage of the man.](https://tramberend.bht-berlin.de/){.iframe width="80%"}

## PDF object

![](../decks/columns.pdf){.pdf w:h="2/1"}

## Mview iframe

``` markdown
![The Utah teapot](assets/teapot.off)
```

renders as

![The Utah teapot](../decks/assets/teapot.off)

# Code

## Code block with caption

``` javascript
function rainbow(numOfSteps, step) {
  // This function generates vibrant, "evenly spaced" colours (i.e. no clustering). This is ideal for creating easily distinguishable vibrant markers in Google Maps and other apps.
  // Adam Cole, 2011-Sept-14
  var r, g, b;
  var h = step / numOfSteps;
```

Caption: Captioned code.

More `function CODE(here) {return true;}`{.javascript} here.

## Selected line numbers

``` {.html line-numbers="1-4,6,7"}
<div class="media">
  <figure class="image">
    <img src="include/06-metal.png" alt="06-metal.png" />
    <figcaption>
      Alt Caption <span class="math inline">\(e=mc^2\)</span>
    </figcaption>
  </figure>
</div>
```

Caption: General structure of all media embeddings

# Rendered code

## From a file

![](../decks/graph.dot){.dot .render align="right" width="50%"}

Caption: Graphviz is cool.

## From a code block

``` {.dot .render align="right" width="50%"}
digraph {
  node [style = filled]
  A [fillcolor = yellowgreen]
  A -> B
  A -> C
  C -> D
  C -> E
  C -> F
  B -> D
}
```

Caption: Graphviz is cool.

## Inline

*Really* tiny: ![](../decks/graph.dot){.dot .render height="3em"}
![](../decks/graph.dot){.dot .render height="2em"} ![](../decks/graph.dot){.dot
.render height="1em"}

# Executed JavaScript

## From a file

![Runs from a file.](../static/es6-blue.js){.javascript .run}

## From a code block

``` {.javascript .run}
import * as mod from "../static/es6.js";
mod.hello(anchor, "orange");
```

Caption: What?

## Inline

-   Animation ![](../static/es6-blue.js){.javascript .run} is followed by text
