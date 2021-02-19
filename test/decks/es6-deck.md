---
title: ES6 Modules in Code Blocks
---

# By hand

::: {#sh62fss45f}
:::

<script type="module">
let anchor = document.getElementById("sh62fss45f");
import *  as mod from "/test/static/es6.js";
mod.hello(anchor, "green");
</script>

# One more

::: {#sh62fss45e}
:::

<script type="module">
let anchor = document.getElementById("sh62fss45e");
import *  as mod from "/test/static/es6.js";
mod.hello(anchor, "red");
</script>

# Decker inline

``` {.javascript .run}
import *  as mod from "/test/static/es6.js";
mod.hello(anchor, "orange");
```

Caption: Fuck YEAH!

# Decker include

![Fuck YEAH!](/test/static/es6-blue.js){.run width="100%"}
