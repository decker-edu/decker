---
css:
- 'columns.css'
history: True
---

# Slide Columns

## Mechanisms

-   Header 3 markers (`###`)
-   Box classes (`.split` and `.join`)
-   Slide masters (`layout` slide attribute)

------

# Header 3 Markers

------

# Header 3 Markers

## Syntax

-   Each H3 mark (`###`) starts a new column
-   Columns are generated as DIVs

### 

## Semantics

-   The slide header is not part of the columns
-   Columns vertically span the entire slide
-   Space is equally distributed between all columns

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

## Example {.right}

![](include/06-metal.png)

------

# Slide Masters {layout="columns"}

## Image Mania {.top}

- Even in *three* columns

## Example {.left}

![](include/06-metal.png)

## Example {.center}

![](include/06-metal.png)

## Example {.right}

![](include/06-metal.png)

