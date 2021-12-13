---
highlight-style: pygments
highlightjs: xcode
subtitle: Clean. Consistent. Robust.
title: New Style Media Handling
---

# Slide Structure

``` html
<section class="slide level1 present">
  <div class="decker">
    <div class="alignment">
      <h1>Licht</h1>
      <div class="layout">
        <div class="area">
          <div class="block">
            <h2>Some Header</h2>
            <p>Something or other</p>
          </div>
        </div>
      </div>
    </div>
  </div>
</section>
```

Caption: General structure of all slides

# Media Structure

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

# Development Path {.columns}

## Branch `develop-layout` {.left}

-   Contains new slide and media renderer
-   Is up to date with branch `develop`

## Branch `develop`

-   Reveal 4 and Pandoc 2.14
-   Default resource pack `decker`
-   Resource packs `wburg` and `mario`

## The plan {.right .fragment}

1.  Cherry-pick fixes and features from `master` to `develop`
2.  On `develop-layout`
    -   Port `wburg` CSS
    -   Port `mario` CSS
3.  Merge `develop-master` to `develop`
4.  Merge `develop` to `master`

# Local Image

![Alt Caption $e=mc^2$](include/06-metal.png)

# Local Media in 2 columns {.columns}

## Left {.left}

![Alt Caption $e=mc^2$](include/06-metal.png)

## Right {.right}

``` javascript
function rainbow(numOfSteps, step) {
  // This function generates vibrant, "evenly spaced" colours (i.e. no clustering). This is ideal for creating easily distinguishable vibrant markers in Google Maps and other apps.
  // Adam Cole, 2011-Sept-14
  var r, g, b;
  var h = step / numOfSteps;
```

Caption: Captioned code.

More `function CODE(here) {return true;}`{.javascript} here

--------------------------------------------------------------------------------

# Local Media in 2 columns {.columns}

## Left {.left .danger}

![Alt Caption $e=mc^2$](include/06-metal.png)

## Right {.right .secondary}

``` javascript
function rainbow(numOfSteps, step) {
  // This function generates vibrant, "evenly spaced" colours (i.e. no clustering). This is ideal for creating easily distinguishable vibrant markers in Google Maps and other apps.
  // Adam Cole, 2011-Sept-14
  var r, g, b;
  var h = step / numOfSteps;
```

Caption: Captioned code.

More `function CODE(here) {return true;}`{.javascript} here

--------------------------------------------------------------------------------

# Local Media in 2 columns {.columns}

## Left {.left .definition}

![Alt Caption $e=mc^2$](include/06-metal.png)

## Right {.right .conclusion}

``` javascript
function rainbow(numOfSteps, step) {
  // This function generates vibrant, "evenly spaced" colours (i.e. no clustering). This is ideal for creating easily distinguishable vibrant markers in Google Maps and other apps.
  // Adam Cole, 2011-Sept-14
  var r, g, b;
  var h = step / numOfSteps;
```

Caption: Captioned code.

More `function CODE(here) {return true;}`{.javascript} here

# More Images in 2 columns {.columns}

## Left {.left align="center"}

![Alt Caption $e=mc^2$](include/06-metal.png)

## Also left {align="right"}

![Alt Caption $e=mc^2$](include/06-metal.png)

## Right {.right align="center" grow="2"}

![Alt Caption $e=mc^2$](include/06-metal.png)

## And some more text {align="left"}

-   Block distance fits.

# Blocks {.columns}

## One {.left}

-   One slightly longer line.
-   Should align with neighbors.

## Should align [:fas](arrow-right) {.warning align="right"}

One slightly longer line.

Should `align with`{.javascript} neighbors.

All right! [:fas](arrow-right)

## One slightly longer line. {.right .primary}

One slightly longer line.

Should align with.

## [:fas](arrow-left) Should align {.secondary}

-   One slightly longer line.

-   Should align with neighbors.

[:fas](arrow-left) All right!

## Three {.success .fragment popup="center-up"}

1.  Three 🍔🍔🍔
2.  Four 🍺🍺🍺🍺

## Source code is always nice {.danger .fragment popup="center-down" width="50%"}

``` markdown
## Three {popup="center-up" .fragment .success}

1.  Three 🍔🍔🍔
2.  Four 🍺🍺🍺🍺
```

## How is my driving? {.secondary .fragment popup="center" align="center"}

Too flashy?

Call the style police at

**015773846827**

# Local Image with caption block

![DISCARDED](include/06-metal.png)

Caption: Block Caption $e=mc^2$

# Local Image {.black}

## `{width=50%}`

![Overwriting Block Caption $e=mc^2$](include/06-metal.png){width="50%"}

# Local Image

## `{height=200px align=right}`

![Overwriting Block Caption $e=mc^2$](include/06-metal.png){align="right"
height="200px"}

# HTML for the last one

``` {.html align="center" width="90%"}
<section id="local-image-height200px" class="slide level1">
<div class="decker">
<div class="section level1 alignment">
<h1>Local Image height=200px</h1>
<div class="layout">
<div class="area">
<div class="media" align="right">
<figure class="image" style="height:auto;width:auto;">
<img src="include/06-metal.png" style="height:200px;width:auto;" alt="06-metal.png" />
<figcaption>
Overwriting Block Caption <span class="math inline">\(e=mc^2\)</span>
</figcaption>
</figure>
</div>
</div>
</div>
</div>
</div>
</section>
```

Caption: Code blocks can have captions too.

# Local Image

## `{width=80% height=200px}`

![Overwriting Block Caption $e=mc^2$](include/06-metal.png){align="center"
width="80%" height="200px"}

# Local Inline Media

Some text ![](pacman-perfect-game.mp4){.controls .autoplay height="100px"} some
more ![](../static/es6.js){.code} code.

![One](include/06-metal.png){height="60px"}![Two](include/06-metal.png){height="120px"}![Three](include/06-metal.png){height="240px"}

# HTML for the last one

``` .html
<section id="local-inline-media" class="slide level1">
<div class="decker">
<div class="section level1 alignment">
<h1>Local Inline Media</h1>
<div class="layout">
<div class="area">
<p>Some text <span class="media"><span class="figure video" style="height:auto;width:200px;"><video controls="1" allow="autoplay" data-autoplay="1" style="height:auto;width:100%;" src="pacman-perfect-game.mp4"></video></span></span> some more.</p>
<p><span class="media"><span class="figure image"><img src="include/06-metal.png" alt="06-metal.png" /><span class="figcaption"><span>One</span></span></span></span> <span class="media"><span class="figure image" style="height:auto;width:auto;"><img src="include/06-metal.png" style="height:100px;width:auto;" alt="06-metal.png" /><span class="figcaption"><span>Two</span></span></span></span></p>
</div>
</div>
</div>
</div>
</section>
```

# Iframe

![Homepage of the man.](https://tramberend.beuth-hochschule.de/){.iframe
width="80%"}

# YouTube Video Stream

![Avatar.](youtube:7XX6IEuLP3A){width="80%" height="200px" aspect="16:9"}

# Local Video

![The perfect game.](pacman-perfect-game.mp4){.controls .autoplay width="60%"}

# PDF Object

![](columns.pdf){.pdf height="500px" width="80%"}

# Embeded SVG

![Why is it called dumbell?](dumbell-hard.svg){.embed width="400px"}

# Rendered Code

![](graph.dot){.dot .render align="right" width="50%"}

Caption: Graphviz is cool.

# Rendered CodeBlock

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

# Rendered Code Inline

*Really* TINY shit: ![](graph.dot){.dot .render height="3em"}
![](graph.dot){.dot .render height="2em"} ![](graph.dot){.dot .render
height="1em"}

# Execute Javascript

![Fuck YEAH!](../static/es6-blue.js){.run width="72%"}

# Execute Javascript CodeBlock

``` {.javascript .run width="72%"}
import * as mod from "../static/es6.js";
mod.hello(anchor, "orange");
```

# Execute Javascript Inline

-   Animation ![](../static/es6-blue.js){.javascript .run width="200px"} is
    followed by text

# An Mview Iframe

## Markdown

``` markdown
![The Utah teapot](teapot.off)
```

## Renders as

![The Utah teapot](teapot.off)

--------------------------------------------------------------------------------

# Behind the Iron Curtain

# AST structure synthesis

## Branch master `master`

``` {.haskell .small}
caption = [Str "Caption", Space, Strong [Str "text"]]
html = toHtml caption
block = RawHtml ("<figcaption>" <> html <> "</figcaption>")
```

Caption: Just like PHP in the 90s

## Branch `develop-layout` {.fragment}

``` {.haskell .small}
caption = [Str "Caption", Space, Strong [Str "text"]]
block = tag "figcaption" $ Div nullAttr [Plain caption]
```

Caption: More like JSX in the 20s

--------------------------------------------------------------------------------

# AST structure synthesis

## Conversion status

-   [:far](check-square) Embedded local media (Image, Video, PDF, ...)
-   [:far](check-square) Code blocks (highlighted, rendered, executed)
-   [:far](check-square) Iframes (generic URLs, Mview models)
-   [:far](check-square) Streaming videos (Youtube, Vimeo, Twitch, ...)
-   [:far](square) Quizzes
-   [:far](square) Polls
