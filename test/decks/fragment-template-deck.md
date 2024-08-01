---
subtitle: What's it with the templates?
title: Fragment Templates
---

# Link Templates

[all-params- arg0 arg1 arg2](url "title"){#theid .class1 .class2 attr1="attr1"
attr2="attr2"}

# Codeblock Templates

``` {#theid .class1 .class2 micro="all-params" attr1="attr1" attr2="attr2"}
The Code.
Probably many lines.
```

# Live Coding

``` {.live-code- language="python"}
print(42)
```

# Live Coding Prelude

## Prelude

``` {#prelude.java .live-code-prelude-}
import static java.lang.System.out;
##CODE##
```

```.java
import static java.lang.System.out;
##CODE##
```

## Live Code

``` {.live-code- language="java" template="#prelude.java"}
out.println(42);
```

[§live-code-server](https://codapi.tramberend.de/v1)
