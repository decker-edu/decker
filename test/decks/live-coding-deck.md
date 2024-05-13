---
subtitle: Edit and Execute Code Blocks
template:
  css:
  # - "https://unpkg.com/@antonz/codapi@0.19.0/dist/snippet.css"
  # - "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/base16/solarized-light.min.css"
  - /support/css/hljs.css
  - codapi.css
  js:
  - "https://unpkg.com/@antonz/codapi@0.19.0/dist/snippet.js"
  - "https://cdn.jsdelivr.net/npm/ace-builds@1.33.1/src-min-noconflict/ace.js"
static-resources:
  - codapi-templates
templates:
  live-code: |
    ````{=html}
    <div class="media">
    <figure class="live-code">
    <pre class="language-:(language)"><code id=":(id)" class="language-:(language)">:(code)</code></pre>
    <codapi-snippet sandbox=":(language)" editor="basic" selector="#:(id)" :(attribs)>
    </codapi-snippet>
    <script type="module">
      import {CodeJar} from "https://cdn.jsdelivr.net/npm/codejar@4.2.0/+esm";
      import hljs from 'https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/es/highlight.min.js';
      import :(language) from 'https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/es/languages/:(language).min.js';
      hljs.configure({
        ignoreUnescapedHTML: true
      });
      hljs.registerLanguage(':(language)', :(language));
      let code = document.querySelector("#:(id)");
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

## TODO {.accent4}

-   [x] integrate [CodeJar](https://medv.io/codejar/) for better editing
-   [x] integrate [highlightjs](https://highlightjs.org/) for syntax highlighting

# Python

``` {macro="live-code" language="python"}
msg = "Hello, World!"
print(msg)
```

# Rust

``` {macro="live-code" language="rust"}
fn main() {
   print!("Hello, World!"); 
}
```

# Haskell

``` {macro="live-code" language="haskell"}
main = putStrLn "Hello, World!"
```


# Live Code Macro (Python)

``` {macro="live-code" language="python"}
msg = "Hello World!"
print(msg)
```

# Live Code Macro Template (Haskell)

A simple template is used around the code.

- TODO Indentation based languages do not work yet

``` {macro="live-code" language="haskell" template="../static/text-main.hs"}
let msg0 = "Hello Mario!"
print msg0
```

# Live Code Macro Cell 1 (Haskell)

The next cell depends on this code

- TODO cannot be executed, main is missing

``` {#lcm-cell-1 macro="live-code" language="haskell"}
msg0 = "Hello Mario!"
printMsg0 = do
  print msg0
main = printMsg0
```

# Live Code Macro Cell 2 (Haskell)

This depends on the previous cell

- TODO does not form a working Haskell file

``` {#lcm-cell-2 macro="live-code" language="haskell" depends-on="lcm-cell-1"}
msg1 = "Hello Mario!"
main = do
  printMsg0
  print msg1
```

# Two Dependent Python Cells

``` {#py-cell-1 macro="live-code" language="python"}
msg = "Hello, World!"
print(msg)
```

``` {macro="live-code" language="python" depends-on="py-cell-1"}
msg = "Hello, World!"
print(msg)
```

[@live-code-server](https://codapi.tramberend.de/v1)
