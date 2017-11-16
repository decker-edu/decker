---
history: True
---

# Rendered Code Blocks

Code blocks can be rendered as SVG images

## Formats

-   Graphviz (dot)
-   Gnuplot

# Embedded Graphviz Code (SVG)

## Highlighted

```` {.dot}
``` {.dot .render width="80%"}
digraph {
  node [style = filled]
  A {fillcolor = yellowgreen}
  A -> B 
  A -> C
  C -> D
  C -> E
  C -> F
  B -> D
}
```
````

### 

## Rendered

``` {.dot .render width="80%"}
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

# Embedded Gnuplot (SVG)

## Highlighted

```` {.gnuplot}
``` {.gnuplot .render width="80%"}
set samples 20, 20
set isosamples 20, 20
set hidden3d back offset 1 trianglepattern 3 undefined 1 altdiagonal bentover
set style data lines
set xrange [ -3.00000 : 3.00000 ] noreverse nowriteback
set yrange [ -2.00000 : 2.00000 ] noreverse nowriteback
DEBUG_TERM_HTIC = 119
DEBUG_TERM_VTIC = 119
splot 1 / (x*x + y*y + 1)
```
````

### 

## Rendered

``` {.gnuplot .render}
set samples 20, 20
set isosamples 20, 20
set hidden3d back offset 1 trianglepattern 3 undefined 1 altdiagonal bentover
set style data lines
set xrange [ -3.00000 : 3.00000 ] noreverse nowriteback
set yrange [ -2.00000 : 2.00000 ] noreverse nowriteback
DEBUG_TERM_HTIC = 119
DEBUG_TERM_VTIC = 119
splot 1 / (x*x + y*y + 1)
```
