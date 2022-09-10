---
lork: true
templates:
  test: ":(1) :(2) :(3) [Link](:(url)) :(title)"
  test2: |
    :(1) :(2) :(3) [Link](:(url)) :(title)
title: Template Macros
---

# Template

## Definition

``` yaml
templates:
  test: :(1) :(2) :(3) [Link](:(url)) :(title)
```

## Invocation

``` markdown
[@test arg1 arg2 arg3](the%20url "Title")
```

## Result

[@test arg1 arg2 arg3](the%20url "Title") [@test2 arg1 arg2
arg3](the%20url "Title")
