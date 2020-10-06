---
short-links:
  beuth: 'https://www.beuth-hochschule.de/@@'
  bind:
    notebook: local
  notebook:
    binder: 'https://mybinder.org/v2/gh/monofon/plc-java/master?filepath=@@@'
    local: 'http://localhost:8192/@@?token=plc'
title: Short Links
---
# Short Links {.small}

## Feature description

-   URI scheme based URL shortener with simple templating
-   Calculate effective URL from a template and an interpolated string value
-   The URI scheme names the template
-   The path component is interpolated into the template
-   Template insertion points are marked `@@` for literal interpolation
-   `@@@` URL-encodes the path component before interpolation
-   Multiple template variants are possible per template name
-   Actual variant to use can be bound globally

# Short Links Example {.small}

## YAML meta data

``` {.yaml}
short-links:
  beuth: 'https://www.beuth-hochschule.de/@@'
  notebook:
    binder: 'https://mybinder.org/v2/gh/monofon/plc-java/master?filepath=@@@'
    local: 'http://localhost:8192/@@?token=plc'
  bind:
    notebook: local
```

# Short Links Example {.small}

## Use like this

``` {.markdown}
1.  [](beuth:people/detail/964)
2.  [](notebook:some/path.thing)
3.  [](notebook.binder:some/path.thing)
```

## Should result in

``` {.markdown}
1.  [](https://www.beuth-hochschule.de/people/detail/964)
2.  [](http://localhost:8192/some/path.thing?token=plc)
3.  [](https://mybinder.org/v2/gh/monofon/plc-java/master?filepath=some%2Fpath.thing)
```

# Short Links Example

## Does it?

1.  [`beuth:people/detail/964`](beuth:people/detail/964)
2.  [`notebook:some/path.thing`](notebook:some/path.thing)
3.  [`notebook.binder:some/path.thing`](notebook.binder:some/path.thing)