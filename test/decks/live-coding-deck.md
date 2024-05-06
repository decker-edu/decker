---
subtitle: Edit and Execute Code Blocks
template:
  css:
  - "https://unpkg.com/@antonz/codapi@0.19.0/dist/snippet.css"
  - "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/base16/solarized-light.min.css"
  - codapi.css
  js:
  - "https://unpkg.com/@antonz/codapi@0.19.0/dist/snippet.js"
  - "https://cdn.jsdelivr.net/npm/ace-builds@1.33.1/src-min-noconflict/ace.js"
templates:
  live-code: |
    <codapi-snippet sandbox=":(title)" editor="basic" selector=":(url)"></codapi-snippet>
  live-code-jar: |
    <codapi-snippet sandbox=":(title)" editor="basic" selector=":(url)"></codapi-snippet>
    <script type="module">
      import {CodeJar} from "https://cdn.jsdelivr.net/npm/codejar@4.2.0/+esm";
      let jar = CodeJar(document.querySelector(':(url)'));
    </script>
  live-code-block: |
    ::: media
    <figure class="live-code">
    <pre id=":(rnd-id)" class="language-:(language)"><code class="language-:(language)">:(code)</code></pre>
    <codapi-snippet sandbox=":(language)" editor="basic" selector="#:(rnd-id)"></codapi-snippet>
    <figcaption>
    :(caption)
    </figcaption>
    </figure>
    :::
  live-code-block-jar: |
    ::: media
    <figure class="live-code">
    <pre id=":(rnd-id)" class="language-:(language)"><code class="language-:(language)">:(code)</code></pre>
    <codapi-snippet sandbox=":(language)" editor="basic" selector="#:(rnd-id)"></codapi-snippet>
    <script type="module">
      import {CodeJar} from "https://cdn.jsdelivr.net/npm/codejar@4.2.0/+esm";
      import hljs from 'https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/es/highlight.min.js';
      import :(language) from 'https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/es/languages/:(language).min.js';
      hljs.registerLanguage(':(language)', :(language));
      let jar = CodeJar(document.querySelector('#:(rnd-id)'), hljs.highlightElement);
    </script>
    <figcaption>
    :(caption)
    </figcaption>
    </figure>
    :::
  live-code-block-ace: |
    ::: media
    <figure class="live-code">
    <pre id=":(rnd-id)" class="language-:(language)"><code class="language-:(language)">:(code)</code></pre>
    <codapi-snippet sandbox=":(language)" editor="basic" selector="#:(rnd-id)"></codapi-snippet>
    <script type="module">
      var editor = ace.edit(":(rnd-id)");
      editor.setTheme("ace/theme/monokai");
      editor.session.setMode("ace/mode/:(language)");
    </script>
    <figcaption>
    :(caption)
    </figcaption>
    </figure>
    :::
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
-   [ ] integrate [PrismJS](https://prismjs.com/) for syntax highlighting

# Python

``` {#python-1 .live-code}
msg = "Hello, World!"
print(msg)
```

[@live-code](#python-1 "python")

# Rust

``` {#rust-1 .live-code}
fn main() {
   print!("Hello, World!"); 
}
```

[@live-code](#rust-1 "rust")

# Haskell

``` {#haskell-1 .live-code}
main = putStrLn "Hello, World!"
```

[@live-code](#haskell-1 "haskell")

[@live-code-server](https://codapi.tramberend.de/v1)

# Live Code Macro (Python)

``` {macro="live-code-block" language="python"}
msg = "Hello World!"
print(msg)
```

# Live Code Macro (Haskell)

``` {macro="live-code-block" language="haskell"}
msg = "Hello World!"
main = print msg
```
