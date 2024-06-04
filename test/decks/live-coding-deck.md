---
subtitle: Edit and Execute Code Blocks
template:
  css:
  - "/support/vendor/codapi-js/snippet.css"
  - /support/css/hljs.css
  - /support/css/codapi.css
  js:
  - "/support/vendor/codapi-js/snippet.js"
templates:
  live-code: |
    ````{=html}
    <div class="media">
    <figure class="live-code">
    <pre class="language-:(language)"><code id=":(rnd-id)" class="language-:(language)">:(code)</code></pre>
    <codapi-snippet id=":(id)" engine=":(engine|codapi)" sandbox=":(sandbox|language)" editor="basic" selector="#:(rnd-id)" :(attribs)>
    </codapi-snippet>
    <script type="module">
      import {CodeJar} from "https://cdn.jsdelivr.net/npm/codejar@4.2.0/+esm";
      import hljs from 'https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/es/highlight.min.js';
      import :(language) from 'https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/es/languages/:(language).min.js';
      hljs.configure({
        ignoreUnescapedHTML: true
      });
      hljs.registerLanguage(':(language)', :(language));
      let code = document.querySelector("#:(rnd-id)");
      // console.log("original:\n", code.textContent.split(/\r?\n|\r|\n/g));
      code.setAttribute("autocomplete", "off");
      code.setAttribute("autocorrect", "off");
      code.setAttribute("autocapitalize", "off");
      code.setAttribute("spellcheck", false);
      let highlight = code => {
        code.removeAttribute("data-highlighted");
        hljs.highlightElement(code);
      }
      let jar = CodeJar(code, highlight);
      code.addEventListener("focus", el => highlight(el.target));
      highlight(code);
    </script>
    <figcaption>
    :(caption)
    </figcaption>
    </figure>
    </div>
    ````
  live-code-server: |
    <codapi-settings url=":(url)"> </codapi-settings>
title: Live Coding
lang: en-EN
---

# Live Coding in Code Blocks {.columns}

## {.left}

No Jupyter, but a self-hosted server with standard Docker images that run the
code snippets:

-   [codapi](https://codapi.org)
-   [codapi-js](https://github.com/nalgeon/codapi-js)

The server runs at:

-   <https://codapi.tramberend.de>

## {.right}

Languages (so far):

-   Python
-   Rust
-   Haskell
-   Java

## TODO {.accent4}

-   [x] integrate [CodeJar](https://medv.io/codejar/) for better editing
-   [x] integrate [highlightjs](https://highlightjs.org/) for syntax highlighting

# Python {.columns}

## Live {.left}

``` {macro="live-code" language="python"}
msg = "Hello, World!"
print(msg)
```

## Markdown {.right}

````markdown
``` {macro="live-code" language="python"}
msg = "Hello, World!"
print(msg)
```
````

# Rust {.columns}

## Live {.left}

``` {macro="live-code" language="rust"}
fn main() {
   print!("Hello, World!"); 
}
```

## Markdown {.right}

````markdown
``` {macro="live-code" language="rust"}
fn main() {
   print!("Hello, World!"); 
}
```
````

# Haskell {.columns}

## Live {.left}

``` {macro="live-code" language="haskell"}
main = putStrLn "Hello, World!"
```

## Markdown {.right}

````markdown
``` {macro="live-code" language="haskell"}
main = putStrLn "Hello, World!"
```
````

`language="haskell"` expects a complete Haskell program with a 
proper `main` function.

# Haskell {.columns}

## Live {.left}

``` {macro="live-code" language="haskell" sandbox="ghci"}
let a = 1
let b = 2
a + b
```

## Markdown {.right}

````markdown
``` {macro="live-code" sandbox="ghci" language="haskell"}
let a = 1
let b = 2
a + b
```
````

`language="haskell" sandbox="ghci"` expects a Haskell snippet which is fed to
GHCi instead of a complete Haskell program.


# Java {.columns}

## Live {.left}

``` {macro="live-code" language="java"}
record FortyTwo(int value) {}
var ft = new FortyTwo(42);
System.out.println(ft);
```

## Markdown {.right}

````markdown
``` {macro="live-code" language="java"}
record FortyTwo(int value) {}
var ft = new FortyTwo(42);
System.out.println(ft);
```
````

`language="java"` expects a Java snippet which is fed to
`jshell`. Also, `jshell` needs a lot of memory and is dog slow.

# JavaScript {.columns}

## Live {.left}

``` {macro="live-code" language="javascript" engine="browser"}
console.log("42")
```

## Markdown {.right}

````markdown
``` {macro="live-code" language="javascript" engine="browser"}
console.log("42")
```
````

`language="javascript" engine="browser"` executes the code directly in the browser.

# Live Code with Template (Haskell) {.columns}

## {.top}

A simple template is used around the code.

## The code {.left}

``` {macro="live-code" language="haskell" template="../static/live-code-template.hs.txt"}
print (mario <> mario <> mario)
```

## The template {.right}

```haskell
import Data.Text
mario = "MARIO"
main = do
  print "the template added this"
  
  ##CODE##
  
  print "the template added this"
```

# Two Dependent Haskell Cells {.columns}

## {.left}

The next cell depends on this code

``` {#lcm-cell-1 macro="live-code" language="haskell" sandbox="ghci"}
a = 1
b = 2
a + b
```

## {.right}

This depends on the previous cell

``` {#lcm-cell-2 macro="live-code" language="haskell" sandbox="ghci" depends-on="lcm-cell-1"}
c = 3
a + b + c
```

# Two Dependent Python Cells {.columns}

## Independent cell {.left}

``` {#py-cell-1 macro="live-code" language="python"}
print("Hello, World 0!")
```

## Depends on left cell{.right}
 
``` {macro="live-code" language="python" depends-on="py-cell-1"}
print("Hello, World 1!")
```

# Plot SVG

``` {macro="live-code" language="python" output-mode="svg"}
import io
import numpy as np
import matplotlib.pyplot as plt

data = {"a": np.arange(50), "c": np.random.randint(0, 50, 50), "d": np.random.randn(50)}
data["b"] = data["a"] + 10 * np.random.randn(50)
data["d"] = np.abs(data["d"]) * 100

plt.scatter("a", "b", c="c", s="d", data=data)
plt.xlabel("entry a")
plt.ylabel("entry b")
plt.show()

stream = io.StringIO()
plt.savefig(stream, format="svg")
print(stream.getvalue())
```

[@live-code-server](https://codapi.tramberend.de/v1)
