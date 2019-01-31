---
author: Henrik
bibliography: '/test/decks/bibliography.bib'
subtitle: Proposal
title: Smart Image URLs
write-back:
  enable: True
---

--------------------------------------------------------------------------------

# Executive Summary {#d6eq}

There are currently a few different ways Decker extends the capabilities of
Pandoc and Reveal.js with respect to image and media handling. This document
proposes a URI based mechanism that would unify macros, image and media handling
utilizing Pandocs representation of Markdowns image markup.

Markdown uses image markup (`![]()`) to specify embedding of images in standard
browser supported formats. Decker controls the precise semantics of image tags
with parameters that can be specified by the writer in three mostly independent
dimensions:

-   Context
-   URI scheme
-   Attributes (class and key-value)

--------------------------------------------------------------------------------

# Image Tag Context {#a86p}

The context in which an image tag is used influences the interpretation of the
image attributes and the rendered HTML. There are three possible contexts:

Inline

:   The image tag is adjacent to other inline elements in a non-header block.

    ``` {.markdown}
    some text ![](folder/image.png) some text.
    ```

    The image is meant to be rendered *inline*, meaning as part of the line the
    tag is embedded in. The image is scaled to fit the line height by default.

Paragraph

:   The image tag is the sole inline element within a paragraph.

    ``` {.markdown data-noescape="1"}
    A very short single sentence paragraph.

    <mark>![Very funny video.](/folder/video.mp4)</mark>

    Another very short single sentence paragraph.
    ```

    if the image ALT-text is not empty, the image is wrapped in a figure element
    and the text is used as an image caption. The image is rendered like a block
    element and scaled to fit the available width to 100%.

Header

:   The image tag occurs in a header 1 block title (slide title)

    ``` {.markdown}
    # Slide with background image ![](../image.png)
    ```

    The image is used as the background image for the slide.

--------------------------------------------------------------------------------

# Image Tag URI @rfc3986 {#v1a8}

The image URI is used to differentiate several different szenarios for the
construction of the corresponding HTML code.

## Image Tag URI scheme

`<empty>`

:   Relative URIs are resolved against local files. Relative URIs with a leading
    `/` are resolved relative to the decker projects root folder. To provide a
    URI relative to the file system root a `file:` URI is used.

    ``` {.markdown}
    # Slide with background video ![](../movie.mp4)
    ```

    Can be used in *Inline*, *Paragraph* and *Header* contexts.

`file:`

:   `file` URIs are resolved against the local file system. URIs with a leading
    `/` are resolved relative to the file system root.

    ``` {.markdown}
    A very short single sentence paragraph.

    ![](file:/usr/local/share/decker/marketing/video.mp4)
    ```

    Can be used in *Inline*, *Paragraph* and *Header* contexts.

`http:` , `https:` , `ftp:` , `ftps`: , `gopher:`, etc.

:   Remote URI are resolved using the specified transport mechanism. They are
    copied unmodified into the generated HTML elements and are not resolved
    during slide deck compilation. (Optionally, remote content could be resolved
    during compilation and cached locally. This would allow the generated slide
    decks that contain remote references to be presented offline.)

    ``` {.markdown}
    # Slide with background video ![](https://media.server/movie.mp4)
    ```

    Can be used in *Inline*, *Paragraph* and *Header* contexts.

`include:`

:   `include:` URIs are resolved during compilation. The path is resolved
    against the local filesystem. Paths with a leading `/` are resolved relative
    to the decker projects root folder. The retrieved plain text resource is
    insert into the markdown source replacing the entire image tag.

    ``` {.markdown}
    A very short single sentence paragraph.

    ![](include:/common/disclaimer.md)

    Another very short single sentence paragraph.

    ![](include:/common/agb.md#24-30)
    ![](include:/common/agb.md#snippet1)
    ```

    `include:` URIs are only valid in the *Paragraph* context.

`youtube:`, `vimeo:`, `twitch:`, `veer:`, etc.

:   Image tags with streaming service schemes are replaced with appropriate
    embedded HTML elements (mostly IFRAMEs probably) that display the referenced
    content.

    ``` {.markdown}
    # Slide with background video ![](youtube:lMSuGoYcT3s)
    ```

    Can be used in *Paragraph* and *Header* contexts.

`fas:` , `fab:` , `meta:` , etc.

:   Image tags with valid macro schemes are replaced with whatever HTML content
    the macro computes. The URI path, query and fragment components may be used
    as parameters for the macro. This is basically a generalization of the
    streaming schemes presented above.

    ``` {.markdown}
    The Font Awesome ![](fas:ghost) icon represents a ghost.

    This document was compiled with the `history` options set to ![](meta:history).
    ```

    Can be used in *Inline*, *Paragraph* and *Header* contexts.

`render:`

:   The render scheme resolves the given path against the local file system.
    URIs with a leading `/` are resolved relative to the decker projects root
    folder root. The content of the

    ``` {.markdown}
    A very short single sentence paragraph.

    ![](render:/folder/diagram.dot?format=png)

    Another very short single sentence paragraph.

    ![](render:/folder/diagram.dot?format=svg)
    ```

`code:`

:   Basically like `include:` but the included text is set verbatim inside a
    code block with syntax highlighting.

    ``` {.markdown}
    A very short single sentence paragraph.

    ![](code:/src/Filter.hs)

    Another very short single sentence paragraph.

    ![](code:/src/Filter.hs#21-24)
    ![](code:/src/Filter.hs#meta-macro-definition)
    ```

## Image Tag Path Extension

`.jpg`, `.png`

:   Bitmap images.

`.svg`

:   SVG vector images.

`.mp4`

:   H.264 video content.

`<empty>`, `.html`

:   HTML page for display in an IFRAME.

--------------------------------------------------------------------------------

# Image Tag Attributes {#nz01}

`width`

:   Width

`height`

:   Height
