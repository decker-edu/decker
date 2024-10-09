---
meta-data: data/path-resolution-meta.yaml
paths:
  existing-file-absolute: /test/decks/include/06-metal.png
  existing-file-relative: include/06-metal.png
  non-existing-file-absolute: /test/decks/this/path/does/not.exist
  non-existing-file-relative: this/path/does/not.exist
runtime-path-variables:
- paths
title: Path Resolution
---

Document Path: [:meta](documentPath)

# Images

Plain image paths.

| ![include/06-metal.png](include/06-metal.png)
| ![/test/decks/include/06-metal.png](/test/decks/include/06-metal.png)

# Meta Data Values

## Definitions

``` yaml
paths:
    existing-file-relative: include/06-metal.png
    existing-file-absolute: /test/decks/include/06-metal.png
    non-existing-file-relative: this/path/does/not.exist
    non-existing-file-absolute: /test/decks/this/path/does/not.exist
```

## Resolved Values

-   existing-file-relative: [:meta](paths.existing-file-relative)
-   existing-file-absolute: [:meta](paths.existing-file-absolute)
-   non-existing-file-relative: [:meta](paths.non-existing-file-relative)
-   non-existing-file-absolute: [:meta](paths.non-existing-file-absolute)

--------------------------------------------------------------------------------

# Included Markdown

Plain image path in an included Markdown file.

[:include](include/path-resolution-include.md)

