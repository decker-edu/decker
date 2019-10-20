---
history: True
center: True
title: Column Layout
subtitle: On decker slides
author: Henrik
date: 2019-10-20
---
# Slide Columns

## Mechanisms

-   Box classes (`.split` and `.join`)
-   Slide masters (`layout` slide attribute)

---

# Box Classes

---

# Box Classes

## Intro

-   No columns here, just a very long line spanning the entire slide

## Syntax {.split}

-   Box headers (`##`) are marked with CSS classes
    -   `split` starts 2 columns
    -   `join` forces 1 column

## Semantics

-   CSS3 column attributes are used for breaking
-   Breaks occur on box borders

## Outro {.join}

-   No columns here, just a very long line spanning the entire slide

---

# Box Classes

-   No columns here
-   But possibly very long lines, longer than any column is wide

## Syntax {.split}

-   Box headers (H2) are marked with CSS classes
    -   `split` starts 2 columns
    -   `join` forces 1 column

## Image

![](include/06-metal.png)

---

# Boxed Box Classes

## Image {.split .def}

![](include/06-metal.png)

## Image {.ale}

![](include/06-metal.png)

## Image {.note}

![](include/06-metal.png)

## Image {.observation}

![](include/06-metal.png)

---

# Slide Masters

---

# Slide Masters {.columns}

## Fixed Layouts {.top}

-   Just like Powerpoint and Keynote

## Syntax {.left}

-   Class and key-value attributes
-   Assign content to layout area
-   On elements *Header 2*, *Image*, *Code Block*, *DIV*

## Example {.right grow="1"}

![](include/06-metal.png)

---

# Slide Masters

## Example slide with two columns and a footer

# Two columns and a footer {.columns}

## Left column {.left}

Some text

## Right column {.right}

Some text

## Footer {.bottom}

Some final text

---

# Slide Masters {.columns}

## Fixed Layouts {.bottom}

-   Just like Powerpoint and Keynote (This is full width again)

## Syntax {.left grow="2"}

-   Class and key-value attributes
-   Assign content to layout area
-   On elements *Header 2*, *Image*, *Code Block*, *DIV*

## Second box

-   In this extra wide column that reaches the left image border

##  {.right}

![](include/06-metal.png)

---

# Slide Masters {.columns}

## Image Mania {.top}

-   Even in *three* columns

## Example {.left}

![](include/06-metal.png)

## Example {.center}

![](include/06-metal.png)

## Example {.right}

![](include/06-metal.png)

---

# Slide Masters {.columns}

## Box Mania {.top}

-   Even in *three* columns

## Example {.left .answer}

![](include/06-metal.png)

## Example {.center .def}

![](include/06-metal.png)

Some more Text after the picture.

## 

And some more still after the box.

## Example {.right .note}

![](include/06-metal.png)

---

# Slide Masters {.columns}

## Example {.left .answer}

![](include/06-metal.png)

## Example {.answer}

![](include/06-metal.png)

## Example {.center .def}

![](include/06-metal.png)

## Example {.answer}

![](include/06-metal.png)

## Example {.right .note}

![](include/06-metal.png)

## Example {.answer}

![](include/06-metal.png)

---

# Slide Masters {.columns}

## Box Mania {.top}

-   Also in *two* columns

## Example {.left .def}

![](include/06-metal.png)

Some more Text

## Example {.right .note}

![](include/06-metal.png)

## 

Some more Text

# Slide Masters {.grid}

##  {.top-left}

![](include/06-metal.png)

##  {.top}

![](include/06-metal.png)

##  {.top-right}

![](include/06-metal.png)

##  {.left}

![](include/06-metal.png)

##  {.center}

![](include/06-metal.png)

##  {.right}

![](include/06-metal.png)

##  {.bottom-left}

![](include/06-metal.png)

##  {.bottom}

![](include/06-metal.png)

##  {.bottom-right}

![](include/06-metal.png)

# Mario Style Columns

## 1 1

::: {.col}
## Left column

This is the left column

![](include/06-metal.png)
:::

::: {.col}
## Right column

This is the right column

![](include/06-metal.png)
:::