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
``` {.live-code- language="Python"}
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

[§live-code-server](https://tramberend.bht-berlin.de/codapi/v1)

# Result: below (legacy) {.columns}

## Live {.left}

``` {.live-code- language="Python" result="below"}
# Squares and cubes
def table(n):
    for i in range(1, n + 1):
        print(f"{i:2} {i*i:4} {i**3:5}")

print(" i   i²    i³")
table(15)
```

This paragraph is pushed down when the result appears.

## Markdown {.right}

```` markdown
``` {.live-code- language="Python" result="below"}
...
```
````

-   Result is added below the code
-   Slide grows and overflows

# Result: split {.columns}

## Live {.left}

``` {.live-code- language="Python" result="split"}
# Squares and cubes
def table(n):
    for i in range(1, n + 1):
        print(f"{i:2} {i*i:4} {i**3:5}")

print(" i   i²    i³")
table(15)
```

This paragraph stays put.

## Markdown {.right}

```` markdown
``` {.live-code- language="Python" result="split"}
...
```
````

-   Frame keeps its pre-run height
-   Code shrinks and scrolls, result gets up to 50%
-   Code and result visible together

# Result: flip {.columns}

## Live {.left}

``` {.live-code- language="Python" result="flip"}
# Squares and cubes
def table(n):
    for i in range(1, n + 1):
        print(f"{i:2} {i*i:4} {i**3:5}")

print(" i   i²    i³")
table(15)
```

This paragraph stays put.

## Markdown {.right}

```` markdown
``` {.live-code- language="Python" result="flip"}
...
```
````

-   Result replaces the code in the same box
-   ✕ flips back to the code, *Output* flips to the result again
-   Result gets the full box height

# Result: overlay {.columns}

## Live {.left}

``` {.live-code- language="Python" result="overlay"}
# Squares and cubes
def table(n):
    for i in range(1, n + 1):
        print(f"{i:2} {i*i:4} {i**3:5}")

print(" i   i²    i³")
table(15)
```

This paragraph stays put.

## Markdown {.right}

```` markdown
``` {.live-code- language="Python" result="overlay"}
...
```
````

-   Result slides over the lower part of the code
-   Fades out while the code has the focus
-   ✕ closes it

# Result: float {.columns}

## Live {.left}

``` {.live-code- language="Python" result="float"}
# Squares and cubes
def table(n):
    for i in range(1, n + 1):
        print(f"{i:2} {i*i:4} {i**3:5}")

print(" i   i²    i³")
table(15)
```

This paragraph is covered by the result, but does not move.

## Markdown {.right}

```` markdown
``` {.live-code- language="Python" result="float"}
...
```
````

-   Result floats over the content below the code
-   Opens upwards if there is no room below
-   ✕ closes it

# Result: reserve {.columns}

## Live {.left}

``` {.live-code- language="Python" result="reserve" result-height="4em"}
# Squares and cubes
def table(n):
    for i in range(1, n + 1):
        print(f"{i:2} {i*i:4} {i**3:5}")

print(" i   i²    i³")
table(15)
```

This paragraph stays put.

## Markdown {.right}

```` markdown
``` {.live-code- language="Python" result="reserve" result-height="4em"}
...
```
````

-   Empty result area is always shown
-   Fixed height, set with `result-height`
-   Layout is the same before and after the run

# Result: auto (default) {.columns}

## Small {.left}

``` {.live-code- language="Python"}
print("Hello, World!")
print("Hello, again!")
```

Small boxes flip.

## Tall {.right}

``` {.live-code- language="Python"}
# Squares and cubes
def table(n):
    for i in range(1, n + 1):
        print(f"{i:2} {i*i:4} {i**3:5}")

print(" i   i²    i³")
table(15)
```

Tall boxes (six lines and up) split.
