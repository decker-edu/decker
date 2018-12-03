---
decker-slide-ids: True
history: True
subtitle: High Quality Slide Annotations
title: Sketch Pad
---

------

# Automatic Slide Id Generation {#s5zu}

## Id Generation

-   If meta data attribute `decker-slide-ids` is set
    1.  If a slide has an id, it is not changed
    2.  Otherwise a 4-digit id is generated
    3.  **The markdown source is written back to file**

## Markdown Write Back

-   Only makes sense if the text editor has auto-reload enabled
-   Forces a rigid formatting of the slide source code
-   Not everybody will like this

------

# First Slide {#myslide-dont-touch}

-   This slide has a hand-crafted id
-   It will not change during further id generation
-   It can be used in links (See [two slides up](#s5zu)) as usual

------

#  {#j355}

-   This slide has no header, but a generated id

------

# Third Slide {#s5zu}

-   This slide has a generated id
-   It will not change during further id generation
-   It can be used in links (See [two slides down](#myslide-dont-touch)) as usual

[:include](./include/something.md)
