---
jupyter:
  kernelspec:
    display_name: Haskell
    language: haskell
    name: haskell
  language_info:
    codemirror_mode: ihaskell
    file_extension: .hs
    name: haskell
  nbformat: 4
  nbformat_minor: 2
title: Write to Jupyter notebook
write-back:
  enable: true
write-notebook: true
---


# Write deck to notebook {#write-deck-to-notebook .columns}

## Decker metadata {#decker-metadata .left}

``` yaml
write-notebook: True
```

##  {#section .right}

## Jupyter metadata

``` yaml
jupyter:
  nbformat: 4
  nbformat_minor: 2
  kernelspec:
     display_name: Haskell
     language: haskell
     name: haskell
  language_info:
     codemirror_mode: "ihaskell"
     file_extension: ".hs"
     name: "haskell"
     pygments_lexer: "Haskell"
     version: "8.6.5"
```

# In the notebook {#in-the-notebook .notebook}

## Selected content

-   Only slides marked with `{.notebook}`

## Code block as cells

-   Code blocks marked with `{.code}`

# A live code cell (Haskell) {#a-live-code-cell-haskell .notebook}

This cell will be live in the notebook.

``` {.haskell .code}
main = putStrLn "Hello World!"
main
```
