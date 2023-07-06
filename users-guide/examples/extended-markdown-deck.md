# Textabschnitt

In diesem Paragraphen ist [dieser Text]{style="color: red;"} rot.

# Abgegrenzter Bereich

Dieser Paragraph ist vor dem Bereich im Quelltext notiert.

::: {style="background-color: salmon;"}

Diese beiden Paragraphen werden durch einen Bereich gruppiert. 

Der Bereich besitzt zur besseren Visualisierung eine andere Hintergrundfarbe.

:::

Dieser Paragraph ist nach dem Bereich im Quelltext notiert.

# Verschachtelte Bereiche

::: {style="background-color: salmon; padding: 0.5rem;"}

Dieser Paragraph ist Teil des äußeren Bereichs.

::: {style="background-color: skyblue;"}

Dieser Paragraph ist Teil des inneren Bereichs.

Der äußere Paragraph besitzt ein kleines Polster zum visualisieren der Verschachtelung.

:::

Dieser Paragraph ist erneut Teil des äußeren Bereichs.

:::

# Unterüberschriften mit Attributen

Text

## Unterüberschrift { style="background-color: salmon;" }

Text

## Unterüberschrift { style="background-color: skyblue;" }

Text

# Matheformeln mit MathJax

Sei $x \in \mathbb{R} \setminus {0}$, so gilt ...

Es gelte \\(n \\in \\mathbb{Z}\\) und \\(m \\in \\mathbb{N}\\) ...

$$ f(x) = \frac{\cos(x)}{\sin(x + \pi)} $$

# Statistiken mit Chart.js

``` {.line-chart width="512px" }
January, February, March, April, May, June, July, August, September, October, November, December
James Smith,-52.0,59.0,-61.0,-80.0,56.0,-75.0,-40.0,45.0,-49.0,58.0,-68.0,70.0
Derek Jones, 98.0,-38.0,82.0,-54.0,-34.0,27.0,90.0,-36.0,60.0,-45.0,40.0,35.0
```

# Vorübersetzter LaTeX-Code

::: columns-1-1

``` { .tex style="font-size: 1.5rem;" }
\documentclass{standalone}
\usepackage{tikz}
\usetikzlibrary{arrows,automata}
\usepackage{pgfplots}
\usepackage{verbatim}
\begin{document}
\begin{tikzpicture}
\begin{axis}
\addplot3[
    surf,
]
{exp(-x^2-y^2)*x};
\end{axis}
\end{tikzpicture}
\end{document}
```

``` { .tex .render height=500px }
\documentclass{standalone}
\usepackage{tikz}
\usetikzlibrary{arrows,automata}
\usepackage{pgfplots}
\usepackage{verbatim}
\begin{document}
\begin{tikzpicture}
\begin{axis}
\addplot3[
    surf,
]
{exp(-x^2-y^2)*x};
\end{axis}
\end{tikzpicture}
\end{document}
```

:::

# Vorübersetzter gnuplot-Code

::: columns-1-1

``` {.gnuplot style="font-size: 0.75rem; width: 512px" }
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

``` {.gnuplot .render height=500px }
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

:::

# Vorübersetzter graphviz-Code

::: columns-1-1

``` {.dot style="font-size: 1.25rem; width: 512px;" }
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

``` {.dot .render height=500px }
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

:::

# Ausklappbare Bereiche

::: columns-1-1

::: { .details summary="Klappentext" }

Ausklappbarer Inhalt

:::

::: { .details summary="Klappentext" open="" }

Ausklappbarer Inhalt

:::

:::

# Spaltenlayout

Es folgt ein dreispaltiger Bereich mit Größenverhältnis 2:3:2

::: columns-2-3-2

::: {style="background-color: salmon;"}

Inhalt erste Spalte

:::

::: {style="background-color: skyblue;"}

Inhalt zweite Spalte

:::

::: {style="background-color: lightgreen;"}

Inhalt dritte Spalte

:::

:::
