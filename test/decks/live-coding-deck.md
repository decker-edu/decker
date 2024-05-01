---
reveal:
  zoom: false
template:
  css:
  - "https://unpkg.com/@antonz/codapi@0.19.0/dist/snippet.css"
  - codapi.css
  - "https://cdn.jsdelivr.net/npm/prismjs@1.29.0/themes/prism.min.css"
  js:
  - "https://unpkg.com/@antonz/codapi@0.19.0/dist/snippet.js"
title: Live Coding
subtitle: Edit and Execute Code Blocks
---

# Live Coding in Code Blocks

No Jupyter, but a self-hosted server with standard Docker images that run the
code snippets:

-   [codapi](https://codapi.org)
-   [codapi-js](https://github.com/nalgeon/codapi-js)

The server runs at:

-   <https://codapi.tramberend.de>

Languages (so far):

-   Python
-   Rust
-   Haskell

## TODO {.accent4}

-   integrate [CodeJar](https://medv.io/codejar/) for better editing
-   integrate [PrismJS](https://prismjs.com/) for syntax highlighting

# Python

``` {#python-1 .live-code}
msg = "Hello, World!"
print(msg)
```

<codapi-snippet sandbox="python" editor="basic" selector="#python-1"></codapi-snippet>

<script type="module">
  // import {CodeJar} from "https://cdn.jsdelivr.net/npm/codejar@4.2.0/+esm";
  // import Prism from 'https://cdn.jsdelivr.net/npm/prismjs@1.29.0/+esm';
  // let jar = CodeJar(document.querySelector('#python-1'), Prism.highlightElement);
</script>

# Rust

``` {#rust-1 .live-code}
fn main() {
  print!("Hello, World!");
}
```

<codapi-snippet sandbox="rust" editor="basic" selector="#rust-1"></codapi-snippet>

# Haskell

``` {#haskell-1 .live-code}
main = putStrLn "Hello, World!"
```

<codapi-snippet sandbox="haskell" editor="basic" selector="#haskell-1"></codapi-snippet>

<codapi-settings url="https://codapi.tramberend.de/v1"> </codapi-settings>
