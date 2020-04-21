---
title: Custom LaTeX Macros
controls: true
menu: true
history: true
---

# Custom LaTeX Macros {layout=columns}

## macros {.left}

$\vc{vc}$    
$\abs{abs}$   
$\norm{norm}$  
$\det{det}$  
$\mt{mt}$  
$\qt{qt}$  
$\pt{pt}$  
$\textcolor{red}{red}$  
$\textcolor{red}red$

## Should look like this image {.right}

![](include/latex_macros.png){width=50%}


# Test

\begin{align*}
  f(x) &= x^2\\
  g(x) &= \frac{1}{x}\\
  F(x) &= \int^a_b \frac{1}{3}x^3
\end{align*}

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
Notes with math!
$$\sum_i \pi^i \to \infty$$
:::


# test5

Test speaker notes again.

::: notes
Once more:
$$\sum_i \pi^i \to \infty$$
:::
