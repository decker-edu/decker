# Introduction

[\#include](include/relative.md)

This is just a simple run-of-the-mill document. Use it for exercises and
everything else. This document also has access to the meta data in surrounding
YAML files.

The date is {{date}}.

All structured data entries are:

{{\#structured}} - {{.}} {{/structured}}

{{sometext}}

This page is published via `rsync` to:

    {{rsync-destination.host}}:{{rsync-destination.path}}

Syntax highlighting should be supported, even with the new bootstrap based
styling.

``` {.haskell}
-- | Monadic version of list concatenation.
(<++>) :: Monad m => m [a] -> m [a] -> m [a]
(<++>) = liftM2 (++)
```
