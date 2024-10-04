---
subtitle: Edit and Execute Code Blocks
title: Live Coding (with fragment templates)
lang: en-EN
---

# Live Coding in Code Blocks {.columns}

## {.left}

No Jupyter, but a self-hosted server with standard Docker images that run the
code snippets:

-   [codapi](https://codapi.org)
-   [codapi-js](https://github.com/nalgeon/codapi-js)

The server runs at:

-   <https://codapi.tramberend.de>

## {.right}

Languages (so far):

-   Python
-   Rust
-   Haskell
-   Java

## TODO {.accent4}

-   [x] integrate [CodeJar](https://medv.io/codejar/) for better editing
-   [x] integrate [highlightjs](https://highlightjs.org/) for syntax highlighting

# Python {.columns}

## Live {.left}

``` {.python .live-code- language="python"}
msg = "Hello, World!"
print(msg)
```

## Markdown {.right}

````markdown
``` {.python .live-code- language="python"}
msg = "Hello, World!"
print(msg)
```
````


