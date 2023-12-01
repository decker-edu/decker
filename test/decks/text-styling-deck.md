---
title: Text Size Test Deck
---

# LaTeX-esque Font Sizes:

::: columns-1-1

::: {}

- [miniscule]{.miniscule}
- [tiny]{.tiny}
- [scriptsize]{.scriptsize}
- [footnotesize]{.footnotesize}
- [small]{.small}
- [normalsize]{.normalsize}
- [large]{.large}

:::

::: {}

- [Large]{.Large}
- [LARGE]{.LARGE}
- [huge]{.huge}
- [Huge]{.Huge}
- [HUGE]{.HUGE}

:::

:::

# Text Color

- [accent0]{.accent0}
- [accent1]{.accent1}
- [accent2]{.accent2}
- [accent3]{.accent3}
- [accent4]{.accent4}
- [accent5]{.accent5}
- [accent6]{.accent6}
- [accent7]{.accent7}

# Text Shade

- [shade0]{.shade0}
- [shade1]{.shade1}
- [shade2]{.shade2}
- [shade3]{.shade3}
- [shade4]{.shade4}
- [shade5]{.shade5}
- [shade6]{.shade6}
- [shade7]{.shade7}

# Normal sizes to compare to

::: dummy

| Table | Table | Table |
| :---: | :---: | :---: |
| What  | What  | What  |

:::

There is some text that wants to be scaled up.

# Scaled with CSS: transform

::: {style='transform: scale(2); transform-origin: "top"'}

| Table | Table | Table |
| :---: | :---: | :---: |
| What  | What  | What  |

:::

There is some text that wants to be [scaled up]{style='display: inline-block; transform: scale(2); transform-origin: "left"'}.

# Scaled with Scalebox-Mechanism

::: {scale=2}

| Table | Table | Table |
| :---: | :---: | :---: |
| What  | What  | What  |

:::

There is some text that wants to be [scaled up]{scale=2}.

# It needs some tricks to properly recenter content

::: {.center-content}

::: {scale=2}

| Table | Table | Table |
| :---: | :---: | :---: |
| What  | What  | What  |

:::

:::

There is some text that wants to be [scaled up]{scale=2}.

# Explanation of the mechanism {.sub}

Take any element with the scale attribute and take its `clientBoundingBox`.

From the box take width and height and divide it by the current slide scale to get its original width and height.

Scale up the original width and height by the scale factor.

Create a wrapper object with the scaled up width and height of the element.

Add the actual `transform: scale(x)` on the element. Because the wrapper has the size of the element after transformation it reflows and rearranges all elements on the slide.

# Scaled with font-size

::: {.huge}

| Table | Table | Table |
| :---: | :---: | :---: |
| What  | What  | What  |

:::

There is some text that wants to be [scaled up]{.huge}.

# Width relative to page size 

::: {.fullpage .block .accent3}

Class: .fullpage

:::

::: {.halfpage .block .accent3}

Class: .halfpage

:::

::: {width="calc(var(--slide-width) * 0.75)" .block .accent3}

`width="calc(var(--slide-width) * 0.75)"`

:::