---
highlight-style: pygments
# highlightjs: xcode
subtitle: Clean. Consistent. Robust.
title: New Style Media Handling
---

# Local Images

![Alt Caption $e=mc^2$](include/06-metal.png)

# Local Inline Media

Some text ![](pacman-perfect-game.mp4){.controls .autoplay height="100px"} some
more ![](../static/es6.js){.code} code.

![One](include/06-metal.png){height="60px"}![Two](include/06-metal.png){height="120px"}![Three](include/06-metal.png){height="240px"}

# Iframe

![Homepage of the man.](https://tramberend.bht-berlin.de/){.iframe
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

