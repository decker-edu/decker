---
title: Rendered Code Blocks
---

# Rendered Code Blocks

Code blocks can be rendered as SVG images

## Formats

-   Graphviz (dot)
-   Gnuplot
-   Tikz

------------------------------------------------------------------------

# Embedded PlantUML Code (SVG) {.columns}

## PlantUML {.left}

```` {.markdown}
``` {.plantuml .render}
@startuml
skinparam shadowing false
skinparam monochrome true
Alice -> Bob: test
@enduml
```
````

## SVG {.right}

``` {.plantuml .render width="60%"}
@startuml
skinparam shadowing false
skinparam monochrome true
Alice -> Bob: test
@enduml
```

# Embedded Mermaid Code (SVG) {layout="columns"}

## Needs `mermaid-cli` to be installed {.top}

```sh
npm install -g @mermaid-js/mermaid-cli
```

## Mermaid {.left}

```` {.markdown}
``` {.mermaid .render}
graph TD
A[Client] --> B[Load Balancer]
```
````

## SVG {.right}

``` {.mermaid .render width="60%"}
graph TD
A[Client] --> B[Load Balancer]
```


------------------------------------------------------------------------

# Embedded Graphviz Code (SVG) {layout="columns"}

## Highlighted {.left}

```` {.markdown}
``` {.dot .render width="80%"}
digraph {
  node [style = filled]
  A {fillcolor = yellow}
  A -> B 
  A -> C
  C -> D
  C -> E
  C -> F
  B -> D
}
```
````

## Rendered {.right}

``` {.dot .render width="80%"}
digraph {
  node [style = filled]
  A [fillcolor = yellow]
  A -> B 
  A -> C
  C -> D
  C -> E
  C -> F
  B -> D
}
```

------------------------------------------------------------------------

# Embedded Gnuplot (SVG) {layout="columns"}

## Highlighted {.left}

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

## Rendered {.right}

``` {.gnuplot .render}
set samples 20, 20
set isosamples 20, 20
set hidden3d back offset 1 trianglepattern 3 undefined 1 altdiagonal bentover
set style data lines
set xrange [ -3.00000 : 3.00000 ] noreverse nowriteback
set yrange [ -2.00000 : 2.00000 ] noreverse nowriteback
DEBUG_TERM_HTIC = 119
DEBUG_TERM_VTIC = 118
splot 1 / (x*x + y*y + 0.5)
```

------------------------------------------------------------------------

# PlantUML Files as Images {layout="columns"}

## Image Tag {.left}

``` {.markdown}
![](alice-bob.plantuml){.render}
```

## Rendered and included {.right}

![](alice-bob.plantuml){.plantuml .render width="60%"}

------------------------------------------------------------------------

# Dot Files as Images {layout="columns"}

## Image Tag {.left}

``` {.markdown}
![](graph.dot){.dot .render}
```

## Rendered and included {.right}

![](graph.dot){.dot .render}

------------------------------------------------------------------------

# Tikz Files as Images {layout="columns"}

## Image Tag {.left}

``` {.markdown}
![](tikz.tex){.render}
```

## Rendered and included {.right}

![](tikz.tex){.tex .render}

------------------------------------------------------------------------

# Tikz Embedded

``` {.tex .render width="50%"}
\documentclass{standalone}
\usepackage{tikz}
\usepackage{verbatim}
\begin{document}
\pagestyle{empty}
\begin{tikzpicture}[scale=3,cap=round]
  % Local definitions
  \def\costhirty{0.8660256}

  % Colors
  \colorlet{anglecolor}{green!50!black}
  \colorlet{sincolor}{red}
  \colorlet{tancolor}{orange!80!black}
  \colorlet{coscolor}{blue}

  % Styles 
  \tikzstyle{axes}=[]
  \tikzstyle{important line}=[very thick]
  \tikzstyle{information text}=[rounded corners,fill=red!10,inner sep=1ex]

  % The graphic
  \draw[style=help lines,step=0.5cm] (-1.4,-1.4) grid (1.4,1.4);

  \draw (0,0) circle (1cm);

  \begin{scope}[style=axes]
    \draw[->] (-1.5,0) -- (1.5,0) node[right] {$x$};
    \draw[->] (0,-1.5) -- (0,1.5) node[above] {$y$};

    \foreach \x/\xtext in {-1, -.5/-\frac{1}{2}, 1}
      \draw[xshift=\x cm] (0pt,1pt) -- (0pt,-1pt) node[below,fill=white]
            {$\xtext$};

    \foreach \y/\ytext in {-1, -.5/-\frac{1}{2}, .5/\frac{1}{2}, 1}
      \draw[yshift=\y cm] (1pt,0pt) -- (-1pt,0pt) node[left,fill=white]
            {$\ytext$};
  \end{scope}

  \filldraw[fill=green!20,draw=anglecolor] (0,0) -- (3mm,0pt) arc(0:30:3mm);
  \draw (15:2mm) node[anglecolor] {$\alpha$};

  \draw[style=important line,sincolor]
    (30:1cm) -- node[left=1pt,fill=white] {$\sin \alpha$} +(0,-.5);

  \draw[style=important line,coscolor]
    (0,0) -- node[below=2pt,fill=white] {$\cos \alpha$} (\costhirty,0);

  \draw[style=important line,tancolor] (1,0) --
    node [right=1pt,fill=white]
    {
      $\displaystyle \tan \alpha \color{black}=
      \frac{ {\color{sincolor}\sin \alpha} }{\color{coscolor}\cos \alpha}$
    } (intersection of 0,0--30:1cm and 1,0--1,1) coordinate (t);

  \draw (0,0) -- (t);
\end{tikzpicture}
\end{document}
```
