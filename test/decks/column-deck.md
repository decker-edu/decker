---
history: True
---

# Slide Columns

## Mechanisms

-   Header 3 markers (`###`)
-   Box classes

## Header 3 Markers

## Syntax

-   Each H3 mark starts a new column
-   Columns are generated as DIVs

### 

## Semantics

-   The slide header is not part of the columns
-   Columns vertically span the entire slide
-   Space is equally distributed between all columns

------

# Box Classes

## Intro

-   No columns here

## Syntax {.split}

-   Box headers (H2) are marked with CSS classes
    -   `split` starts 2 columns
    -   `join` forces 1 column

## Semantics

-   CSS3 column attributes are used for breaking
-   Breaks occur on box borders

## Outro {.join}

-   No columns here

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

# Floating Images

![](include/06-metal.png){.float-right width="50%"}

## No Columns

-   But floating images that
-   force text to flow around them

## Long Long Lines

-   Which results in quite pleasant looking layouts
-   Space utilization appears to be maximized
-   The same could be built for boxes that have a fixed width
