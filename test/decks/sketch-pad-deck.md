---
generate-ids: True
history: True
subtitle: High Quality Slide Annotations
title: Sketch Pad
write-back:
  line-columns: 80
  line-wrap: auto
---

--------------------------------------------------------------------------------

# Automatic Slide Id Generation {#ljkt}

## Id Generation

-   If meta data attribute `generate-ids` is set
    1.  If a slide has an id, it is not changed
    2.  Otherwise a 4-digit id is generated
    3.  **The markdown source is written back to file**

## Markdown Write Back

-   Only makes sense if the text editor has auto-reload enabled
-   Forces a rigid formatting of the slide source code
-   Not everybody will like this

--------------------------------------------------------------------------------

# First Slide {#myslide-dont-touch}

-   This slide has a hand-crafted id
-   It will not change during further id generation
-   It can be used in links (See [two slides up](#s5zu)) as usual

--------------------------------------------------------------------------------

#  {#j355}

-   This slide has no header, but a generated id

--------------------------------------------------------------------------------

# Third Slide {#nqz2}

-   This slide has a generated id
-   It will not change during further id generation
-   It can be used in links (See [two slides down](#myslide-dont-touch)) as
    usual

[:include](./include/something.md)

--------------------------------------------------------------------------------

# Markdown Table Writeback {#e9r1}

## Pipe Table with more than 100 columns

| 123456789 | 123456789 | 123456789 | 123456789 | 123456789 | 123456789 | 123456789 | 123456789 | 123456789 | 123456789 |
|-----------|-----------|-----------|-----------|-----------|-----------|-----------|-----------|-----------|-----------|
|           |           |           |           |           |           |           |           |           |           |

--------------------------------------------------------------------------------

# Markdown Table Writeback {#dh4k}

## Grid Table with more than 100 columns

+------------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+
| 123456789  | 123456789 | 123456789 | 123456789 | 123456789 | 123456789 | 123456789 | 123456789 | 123456789 | 123456789 |
+============+===========+===========+===========+===========+===========+===========+===========+===========+===========+
| -   line 1 |           |           |           |           |           |           |           |           |           |
| -   line 2 |           |           |           |           |           |           |           |           |           |
| -   line 3 |           |           |           |           |           |           |           |           |           |
+------------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+

--------------------------------------------------------------------------------

#  {#n1bq}

|                             |                             |                             |
|-----------------------------|-----------------------------|-----------------------------|
| ![](./include/06-metal.png) | ![](./include/06-metal.png) | ![](./include/06-metal.png) |
| ![](./include/06-metal.png) | ![](./include/06-metal.png) | ![](./include/06-metal.png) |
| ![](./include/06-metal.png) | ![](./include/06-metal.png) | ![](./include/06-metal.png) |

--------------------------------------------------------------------------------

#  {#g4z3}

+-----------------------------+-----------------------------+-----------------------------+
| ![](./include/06-metal.png) | ![](./include/06-metal.png) | ![](./include/06-metal.png) |
+-----------------------------+-----------------------------+-----------------------------+
| ![](./include/06-metal.png) | -   Grid Tables             | ![](./include/06-metal.png) |
|                             | -   Coole Sache             |                             |
|                             | -   Emacs kann das          |                             |
+-----------------------------+-----------------------------+-----------------------------+
| ![](./include/06-metal.png) | ![](./include/06-metal.png) | ![](./include/06-metal.png) |
+-----------------------------+-----------------------------+-----------------------------+
