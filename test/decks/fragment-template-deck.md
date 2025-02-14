---
experiments:
  fragment-templates: true
subtitle: What's it with the templates?
title: Fragment Templates
---

# Fragment Templates

## WAT?

-   *Fragment templates* use the Pandoc template syntax and processor to
    transform Markdown link and code block elements

## Experimental

Fragment templates are hidden behind a flag and need to be explicitly enabled

``` yaml
experiments:
  fragment-templates: true
```

# Template Search Pathes

## In order of preference

1. Document base directory
2. Project's dir `/templates` folder
3. Resource pack's `support/templates` folder

# {.columns}

## Link Templates {.left}

``` markdown
[all-params- arg0 arg1 arg2](url "title"){#theid .class1 .class2 attr1="attr1"
attr2="attr2"}
```

## Macro `all-params-`

``` html
<ol>
<li>arg1: <strong>${arg1}</strong></li>
<li>arg2: <strong>${arg2}</strong></li>
<li>arg3: <strong>${arg3}</strong></li>
<li>url: <strong>${url}</strong></li>
<li>title: <strong>${title}</strong></li>
<li>id: <strong>${id}</strong></li>
<li>class1: <strong>${class1}</strong></li>
<li>class2: <strong>${class2}</strong></li>
<li>args: <strong>${args}</strong></li>
<li>classes: <strong>${classes}</strong></li>
<li>attribs: <strong>${attribs}</strong></li>
<li>code: 
  <pre><code><strong>${code}</strong></code></pre>
</li>
</ol>
```

## {.right}

[all-params- arg0 arg1 arg2](url "title"){#theid .class1 .class2 attr1="attr1"
attr2="attr2"}

# {.columns}

## Codeblock Templates {.left}

```` markdown
``` {#theid .all-params- .class1 .class2 attr1="attr1" attr2="attr2"}
The Code.
Probably many lines.
```
````

## {.right}

``` {#theid .all-params- .class1 .class2 attr1="attr1" attr2="attr2"}
The Code.
Probably many lines.
```

# Live Coding Macro

## Code block

```` markdown
``` {.live-code- language="python"}
print(42)
```
````

## Result

``` {.live-code- .small language="python" caption="Captions are swell!"}
print(42)
```

# Live Coding Prelude

## Prelude

``` {#prelude.java .live-code-prelude-}
import static java.lang.System.out;
##CODE##
```

``` .java
import static java.lang.System.out;
##CODE##
```

## Live Code

``` {.live-code- language="java" template="#prelude.java"}
out.println(42);
```

[§live-code-server](https://codapi.tramberend.de/v1)

# Grid Test {.columns}

## `canvas` {.left}

[§canvas](500px)

## `grid` {.center}

[§grid](500px)

## `finegrid` {.right}

[§finegrid](500px)
