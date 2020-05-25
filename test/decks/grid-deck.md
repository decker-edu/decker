---
title: Grid Layouts
---

# Some ideas on grid layout for slides

## This is not even a proposal

------------------------------------------------------------------------

# Layout templates

## Principle

-   Rectangular named layout areas are positioned on a regular grid

-   Grid size is determined automatically from the layout spec

-   Illegal layout templates generate an inline error

------------------------------------------------------------------------

# Layout templates

## Specification in Yaml

-   Layout templates can be specified in the meta data

-   Example with 3 layout areas in 3 by 3 grid

    ``` {.yaml}
    layouts:
      fancy:
      - h h h h
      - l b b b
      - l b b b
      short: "hhh|bbr|lff"
    ```

------------------------------------------------------------------------

# Using layout templates

## Referenced

-   Boxes are assigned to layout areas by class or attribute

-   Example slide for the body area `b` of above layout

    ``` {.markdown}
    # Slide with layout {layout="fancy"}

    ## This is it {.b}

    - Some text

    ## This is also it

    - Shows up in the body cell too
    ```

------------------------------------------------------------------------

# Using layout templates

## Inline

-   Layout templates can be specified ad hoc in a slide attribute

    ``` {.markdown}
    # Slide with ad hoc layout {layout="hhh|bbr|lff"}

    ## Block allocation is the same {.h}

    - This is the header block
    ```
