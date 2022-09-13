---
lork: true
templates:
  test: ":(1) :(2) :(3) [:(title)](:(url)) :(args)"
  test2: |
    <a href=":(url)">Pony</a> :(args) <a href=":(url)">Unicorn</a>
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

Hallo [@test arg1 arg2 arg3](the%20url "Title") 
Was denn [@test2 There is no](the%20url "Title")
