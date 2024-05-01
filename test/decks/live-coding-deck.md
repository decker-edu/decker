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

<pre id = "python-1" class="live-code">
msg = "Hello, World!"
print(msg)
</pre>

<codapi-snippet sandbox="python" editor="basic" selector="#python-1"></codapi-snippet>

<script type="module">
  import {CodeJar} from "https://cdn.jsdelivr.net/npm/codejar@4.2.0/+esm";
  let jar = CodeJar(document.querySelector('#python-1'));
</script>

# Rust

<pre id = "rust-1" class="live-code">
fn main() {
  print!("Hello, World!");
}
</pre>

<codapi-snippet sandbox="rust" editor="basic" selector="#rust-1"></codapi-snippet>

<script type="module">
  import {CodeJar} from "https://cdn.jsdelivr.net/npm/codejar@4.2.0/+esm";
  let jar = CodeJar(document.querySelector('#rust-1'));
</script>

# Haskell

<pre id = "haskell-1" class="live-code">
main = putStrLn "Hello, World!"
</pre>

<codapi-snippet sandbox="haskell" editor="basic" selector="#haskell-1"></codapi-snippet>

<script type="module">
  import {CodeJar} from "https://cdn.jsdelivr.net/npm/codejar@4.2.0/+esm";
  let jar = CodeJar(document.querySelector('#haskell-1'));
</script>

<codapi-settings url="https://codapi.tramberend.de/v1"> </codapi-settings>
