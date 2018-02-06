---
history: True
---

# Slide Columns

## Mechanisms

-   Box classes (`.split` and `.join`)
-   Slide masters (`layout` slide attribute)

------

# Box Classes

------

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

------

# Box Classes

-   No columns here
-   But possibly very long lines, longer than any column is wide

## Syntax {.split}

-   Box headers (H2) are marked with CSS classes
    -   `split` starts 2 columns
    -   `join` forces 1 column

## Image

![](include/06-metal.png)

------

# Boxed Box Classes

## Image {.split .def}

![](include/06-metal.png){width="100%" height="150px"}

## Image {.ale}

![](include/06-metal.png){width="100%" height="150px"}

## Image {.note}

![](include/06-metal.png){width="100%" height="150px"}

## Image {.observation}

![](include/06-metal.png){width="100%" height="150px"}

------

# Slide Masters

------

# Slide Masters {layout="columns"}

## Fixed Layouts {.top}

-   Just like Powerpoint and Keynote

## Syntax {.left}

-   Class and key-value attributes
-   Assign content to layout area
-   On elements *Header 2*, *Image*, *Code Block*, *DIV*

## Example {.right grow="1"}

![](include/06-metal.png)

------

# Slide Masters {layout="columns"}

## Fixed Layouts {.bottom}

-   Just like Powerpoint and Keynote

## Syntax {.left grow="2"}

-   Class and key-value attributes
-   Assign content to layout area
-   On elements *Header 2*, *Image*, *Code Block*, *DIV*

## Second box

-   In this extra wide column

##  {.right}

![](include/06-metal.png)

------

# Slide Masters {layout="columns"}

## Image Mania {.top}

-   Even in *three* columns

## Example {.left}

![](include/06-metal.png)

## Example {.center}

![](include/06-metal.png)

## Example {.right}

![](include/06-metal.png)

------

# Slide Masters {layout="columns"}

## Box Mania {.top}

-   Even in *three* columns

## Example {.left .alert}

![](include/06-metal.png)

## Example {.center .def}

![](include/06-metal.png)

Some more Text after the picture.

## 

And some more still after the box.

## Example {.right .note}

![](include/06-metal.png)

------

# Slide Masters {layout="columns"}

## Example {.left .alert}

![](include/06-metal.png){width="100%" height="150px"}

## Example {.alert}

![](include/06-metal.png){width="100%" height="150px"}

## Example {.center .def}

![](include/06-metal.png){width="100%" height="150px"}

## Example {.alert}

![](include/06-metal.png){width="100%" height="150px"}

## Example {.right .note}

![](include/06-metal.png){width="100%" height="150px"}

## Example {.alert}

![](include/06-metal.png){width="100%" height="150px"}

------

# Slide Masters {layout="columns"}

## Box Mania {.top}

-   Also in *two* columns

## Example {.left .def}

![](include/06-metal.png)

Some more Text

## Example {.right .note}

![](include/06-metal.png)

## 

Some more Text
