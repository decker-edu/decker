---
generate-ids: True
history: True
publish-annotations: True
sketch-pad-viewer-url: 'http://sketchpad-webview.herokuapp.com/bundle.js'
subtitle: High Quality Slide Annotations
title: Sketch Pad
write-back:
  enable: True
  line-columns: 80.0
  line-wrap: auto
---

---

# Automatic Slide Id Generation {#s5zu}

## Id Generation

-   If meta data attribute `generate-ids` is set
    1.  If a slide has an id, it is not changed
    2.  Otherwise a 4-digit id is generated
    3.  **The markdown source is written back to file**

## Markdown Write Back

-   Only makes sense if the text editor has auto-reload enabled
-   Forces a rigid formatting of the slide source code
-   Not everybody will like this

---

# Sketch Pad Web View {#usee}

## Meta Data

-   Meta data attribute `sketch-pad-viewer-url` points to the Javascript bundle
    for the viewer
-   Has no default value

---

# Annotations {#p5v6}

## Publishing

-   Annotations are pushed into `/annotations`
-   `/anotations` will be published along with the HTML files if
    `publish-annotations` is set to True

---

# First Slide {#myslide-dont-touch}

-   This slide has a hand-crafted id
-   It will not change during further id generation
-   It can be used in links (See [two slides up](#s5zu)) as usual

---

#  {#jcvj}

-   This slide has no header, but a generated id

---

# Third Slide {#we5u}

-   This slide has a generated id
-   It will not change during further id generation
-   It can be used in links (See [two slides down](#myslide-dont-touch)) as
    usual

[:include](./include/something.md)

---

# Markdown Table Writeback {#e9r1}

## Pipe Table with more than 100 columns

| 123456789 | 123456789 | 123456789 | 123456789 | 123456789 | 123456789 | 123456789 | 123456789 | 123456789 | 123456789 |
|-----------|-----------|-----------|-----------|-----------|-----------|-----------|-----------|-----------|-----------|
|           |           |           |           |           |           |           |           |           |           |

---

# Markdown Table Writeback {#dy8d}

## Grid Table with more than 100 columns

\[TABLE\]

---

#  {#n1bq}

|                             |                             |                             |
|-----------------------------|-----------------------------|-----------------------------|
| ![](./include/06-metal.png) | ![](./include/06-metal.png) | ![](./include/06-metal.png) |
| ![](./include/06-metal.png) | ![](./include/06-metal.png) | ![](./include/06-metal.png) |
| ![](./include/06-metal.png) | ![](./include/06-metal.png) | ![](./include/06-metal.png) |

---

#  {#fjm2}
