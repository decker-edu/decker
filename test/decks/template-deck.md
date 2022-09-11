---
lork: true
templates:
  test: ":(1) :(2) :(3) [:(title)](:(url))"
  test2: |
    <a href=":(url)">Pony</a>

    <a href=":(url)">Unicorn</a>
title: Template Macros
---

# Template

## Macro definition

``` yaml
templates:
  test: :(1) :(2) :(3) [:(title)](:(url))
```

## Macro invocation

``` markdown
[@test arg1 arg2 arg3](the%20url "Title")
```

## Result

[@test arg1 arg2 arg3](the%20url "Title")

[@test2 arg1 arg2 arg3](the%20url "Title")
