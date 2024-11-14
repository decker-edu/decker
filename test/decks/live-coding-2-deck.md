---
lang: en-EN
subtitle: Edit and Execute Code Blocks
title: Live Coding (with fragment templates)
experiments:
    fragment-templates: true
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
-   [x] integrate [highlightjs](https://highlightjs.org/) for syntax
    highlighting

# Python {.columns}

## Live {.left}

``` {.python .live-code- language="Python"}
msg = "Hello, World!"
print(msg)
```

## Markdown {.right}

```` markdown
``` {.live-code- language="python"}
msg = "Hello, World!"
print(msg)
```
````

# Haskell {.columns}

## Live {.left}

``` {.live-code- language="Haskell" sandbox="GHCi" height="200px"}
msg = "Hello, World!"
print msg
```

## Markdown {.right}

```` markdown
``` {.live-code- language="Haskell" sandbox="ghci"}
msg = "Hello, World!"
print msg
```
````

[live-code-server-](https://codapi.tramberend.de/v1)
