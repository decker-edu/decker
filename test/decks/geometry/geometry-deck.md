---
template:
  css: ./static/geometry.css
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
import * as g from "./static/geometry.js";
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
import * as g from "./static/geometry.js";

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

------------------------------------------------------------------------

# Labels

``` {.javascript .run}
import * as g from "./static/geometry.js";

let p = g.point(100,350, "drag");
let q = g.point(300,350, "drag");
let r = g.point(600,350, "drag");
let s = g.point(900,350, "drag");

g.renderSvg(anchor, 1200, 500, g.group(
    
    g.label(g.point(100,100), "A", "n"),    
    g.label(g.point(200,100), "B", "ne"),    
    g.label(g.point(300,100), "C", "e"),    
    g.label(g.point(400,100), "D", "se"),    
    g.label(g.point(500,100), "E", "s"),    
    g.label(g.point(600,100), "F", "sw"),    
    g.label(g.point(700,100), "G", "w"),    
    g.label(g.point(800,100), "H", "nw"),    

    g.mlabel(g.point(100,200), "A", "n"),    
    g.mlabel(g.point(200,200), "B", "ne"),    
    g.mlabel(g.point(300,200), "C", "e"),    
    g.mlabel(g.point(400,200), "D", "se"),    
    g.mlabel(g.point(500,200), "E", "s"),    
    g.mlabel(g.point(600,200), "F", "sw"),    
    g.mlabel(g.point(700,200), "G", "w"),    
    g.mlabel(g.point(800,200), "H", "nw"),    

    g.mlabel(p, "A", "n"),    
    g.mlabel(p, "B", "ne"),    
    g.mlabel(p, "C", "e"),    
    g.mlabel(p, "D", "se"),    
    g.mlabel(p, "E", "s"),    
    g.mlabel(p, "F", "sw"),    
    g.mlabel(p, "G", "w"),    
    g.mlabel(p, "H", "nw"),    

    g.label(q, "A", "n" ),    
    g.label(q, "B", "ne"),    
    g.label(q, "C", "e" ),    
    g.label(q, "D", "se"),    
    g.label(q, "E", "s" ),    
    g.label(q, "F", "sw"),    
    g.label(q, "G", "w" ),    
    g.label(q, "H", "nw"),    

    g.mlabel(r, "A", "n" ),    
    g.mlabel(r, "BBBB", "ne"),    
    g.mlabel(r, "CCCC", "e" ),    
    g.mlabel(r, "DDDD", "se"),    
    g.mlabel(r, "E", "s" ),    
    g.mlabel(r, "FFFF", "sw"),    
    g.mlabel(r, "GGGG", "w" ),    
    g.mlabel(r, "HHHH", "nw"),    

    g.label(s, "A", "n" ),    
    g.label(s, "BBBB", "ne"),    
    g.label(s, "CCCC", "e" ),    
    g.label(s, "DDDD", "se"),    
    g.label(s, "E", "s" ),    
    g.label(s, "FFFF", "sw"),    
    g.label(s, "GGGG", "w" ),    
    g.label(s, "HHHH", "nw"),    
));
```

# Infinite Line

``` {.javascript .run}
import * as g from "./static/geometry.js";

let p = g.point(100,150, "drag");
let q = g.point(400,150, "drag");

g.renderSvg(anchor, 500, 300, g.group(
    g.line(p, q, "infinite")
));
```
