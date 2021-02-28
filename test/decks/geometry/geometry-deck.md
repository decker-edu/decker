---
template:
  css: static/geometry.css
title: ES6 Modules in Code Blocks
---

# Code Blocks with a *`run`* class

## Markdown

<style>
#the-markdown-2 {
    background-color: #fa6;
}
#generated-html-4 {
    background-color: #fa6;
}
</style>

```` {#the-markdown .markdown}
``` {.javascript .run}
console.log("Hello", anchor);
```
````

## HTML

``` {#generated-html .html}
<div id="06c02d29c"></div>
<script type="module">
let anchor = document.getElementById("06c02d29c");
console.log("Hello", anchor);
</script>
```

------------------------------------------------------------------------

# Code Blocks with a *`run`* class

## Import any ES6 module(s)

```` {.markdown}
``` {.javascript .run}
import * as g from "/test/decks/geometry/static/geometry.js";
let segment = g.bezier(
    g.point(60, 60),
    g.point(540, 60, "drag"),
    g.point(540, 340, "drag"),
    g.point(60, 340)
);
g.renderSvg(anchor, 600, 400, segment);
```
````

------------------------------------------------------------------------

# Bezier segment {.columns}

##  {.left}

``` {.javascript .run}
import * as g from "/test/decks/geometry/static/geometry.js";

let segment = g.bezier(
    g.point(60, 60),
    g.point(540, 60, "drag"),
    g.point(540, 340, "drag"),
    g.point(60, 340)
);

g.renderSvg(anchor, 600, 400, segment);
```

##  {.right .fragment}

![Make sure to drag the green ones!](./bezier.js){.run width="100%"}

------------------------------------------------------------------------

# Intersection

## 

![](./intersection.js){.run width="100%"}

------------------------------------------------------------------------

# Mirror {.columns}

##  {.left}

![](./mirror.js){.run width="100%"}

##  {.right .fragment}

![$\mathbf{d}_r=-\mathbf{d}_i+2(\mathbf{d}_i\cdot{\mathbf{n}})\mathbf{n}$](./unfold.js){.run
width="100%"}
