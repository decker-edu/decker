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
  enable: false
write-notebook: false
---


# Write deck to notebook[ test/decks/notebook-deck.md]{.document-path} {#write-deck-to-notebook .columns data-source-path="test/decks/notebook-deck.md"}

## Decker metadata {#decker-metadata .left}

``` yaml
write-notebook: True
```

##  {#section .right}

## Jupyter metadata {#jupyter-metadata}

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

# In the notebook[ test/decks/notebook-deck.md]{.document-path} {#in-the-notebook .notebook data-source-path="test/decks/notebook-deck.md"}

## Selected content {#selected-content}

-   Only slides marked with `{.notebook}`

## Code block as cells {#code-block-as-cells}

-   Code blocks marked with `{.code}`

# A live code cell (Haskell)[ test/decks/notebook-deck.md]{.document-path} {#a-live-code-cell-haskell .notebook data-source-path="test/decks/notebook-deck.md"}

This cell will be live in the notebook.

``` {.haskell .code}
main = putStrLn "Hello World!"
main
```
