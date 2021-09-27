---
reveal:
  center: false
title: Spacing and margins
highlightjs: false
highlight-style: pygments
---

#  {.columns}

##  {.left}

Empty header.

Empty everything.

Just some lines of text.

##  {.right}

![Just an image](include/06-metal.png)

# Paragraphs

A short one-line paragraph.

A longer paragraph that `contains a *lot more* words`{.markdown} than the short one-line
paragraph.

A short one-line paragraph.

A longer paragraph that contains a lot more words than the short one-line
paragraph. A longer paragraph that contains a lot more words than the short
one-line paragraph. A longer paragraph that contains a lot more words than the
short one-line paragraph.

# Lists {.columns}

## Solo {.left}

-   Line one is very short.
-   Line two is not much longer.
-   Line three is so long that it certainly wraps.

## Framed {.right}

The same list, but with some text before it

-   Line one is very short.
-   Line two is not much longer.
-   Line three is so long that it certainly wraps.

And some text after it.

# Indented lists {.columns}

## Solo {.left}

-   Line one is very short.
    -   Line two is not much longer.
-   Line three is so long that it certainly wraps.

## Framed {.right}

The same list, but with some text before it

-   Line one is very short.
    -   Line two is not much longer.
-   Line three is so long that it certainly wraps.

And some text after it.

# Paragraph + image {.columns}

## Not tagged {.left}

A short one-line paragraph.

![$e=mc^2$](include/06-metal.png)

A longer paragraph that contains a lot more words than the short one-line
paragraph.

## Tagged {.right .success}

A short one-line paragraph.

![$e=mc^2$](include/06-metal.png)

A longer paragraph that contains a lot more words than the short one-line
paragraph.

# Two blocks in a column

## Looking good

-   Line one is very short.
    -   Line two is not much longer.
-   Line three is so long that it certainly wraps.

## Not so much

-   Line one is very short.
    -   Line two is not much longer.
-   Line three is so long that it certainly wraps.

# Tagged blocks

## Name change! {.warning}

-   They are not called **~~boxes~~** anymore
-   The are called **blocks** now
-   You might want to `%s/\.box/\.block/g`

## Two tagged blocks in a row is tricky

![$e=mc^2$](include/06-metal.png){width="40%"}
