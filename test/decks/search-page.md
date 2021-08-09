---
title: Search
---

# Here be search widget

## Score 0.5

``` {.javascript .run}
import(Decker.meta.supportPath + "/fuzzySearch/search.js")
    .then(module => console.log(module.default(anchor, 0.6)));
```

## Score 0.6

``` {.javascript .run}
import(Decker.meta.supportPath + "/fuzzySearch/search.js")
    .then(module => module.default(anchor, 0.6));
```

## Score 0.7

``` {.javascript .run}
import(Decker.meta.supportPath + "/fuzzySearch/search.js")
    .then(module => module.default(anchor, 0.7));
```
