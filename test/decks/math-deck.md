---
controls: true
menu: true
title: Custom LaTeX Macros
meta-data:
  - math-macros.yaml
---

# Math Fragments: In Code

::: incremental
- Navier-Stokes-Gleichungen
  $$\begin{eqnarray}
    \dot{\vec{u}} &=& 
    \fragment{-\vec{u}\cdot\grad\vec{u}}
    \fragment{\;-\; \frac{1}{\rho}\grad p}
    \fragment{\;+\; \nu \laplace \vec{u}}
    \fragment{\;+\; \vec{f}} 
    \label{eq:momentum} \\[2mm]
    \grad \cdot \vec{u} &=& 0 
    \label{eq:incompressibility}
  \end{eqnarray}$$
- Formeln können schrittweise eingeblendet werden
- Formeln können referenziert und verlinkt werden\
  (siehe nächste Folie)
:::

# Math Fragments: .incremental-math

::: incremental

- Herleitungen an der Tafel sind nicht in Videoaufzeichnung
- Herleitungen auf den Folien sind zu schnell
  [$$
  \begin{eqnarray*}
  a &=& b \\
  a^2 &=& ab \\
  2a^2 &=& a^2 + ab \\
  2a^2-2ab &=& a^2 - ab \\
  2a(a-b) &=& a (a-b) \\
  2a &=& a \\
  2 &=& 1
  \end{eqnarray*}
  $$]{ .math-incremental }
- Die virtuelle Tafel ist ein guter Kompromiss :thumbsup:

:::

::: footer
Hier der Link auf Navier-Stokes-Gleichungen: $\eqref{eq:momentum}$.
:::


# Custom LaTeX Macros {layout=columns}

## macros {.left}

$\vc{vc}$\
$\abs{abs}$\
$\norm{norm}$\
$\mt{mt}$\
$\qt{qt}$\
$\pt{pt}$\
$\textcolor{red}{red}$\
$\textcolor{red}red$

## Should look like this image {.right}

![](include/latex_macros.png){width="50%"}

# Test

`\begin{align*}   f(x) &= x^2\\   g(x) &= \frac{1}{x}\\   F(x) &= \int^a_b \frac{1}{3}x^3 \end{align*}`{=tex}

# test2

$$
  f(x) = x^2 \\
  g(x) = \frac{1}{x}\\
  F(x) = \int^a_b \frac{1}{3}x^3
$$

# test3

$$\lim_{x \to \infty} \exp(-x) = 0$$

# test4

This slide contains a math equation in the speaker notes. Press `s` to show it.

::: notes
Notes with math! $$\sum_i \pi^i \to \infty$$
:::

# test5

Test speaker notes again.

::: notes
Once more: $$\sum_i \pi^i \to \infty$$
:::
