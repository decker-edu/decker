---
css: 'thebe-styling.css'
header-includes: |
    <script type="text/x-thebe-config">
        {
        bootstrap: true,
        requestKernel: true,
        predefinedOutput: false,
        kernelOptions: {
            name: "haskell",
                serverSettings: {
                "baseUrl": "http://localhost:7777/",
                "token": "test-secret"
                }
        },
        selector: "[data-executable]",
        mathjaxUrl: false,
        codeMirrorConfig: {
            mode: "haskell"
        }
        }
    </script>
    <script src="https://unpkg.com/thebelab@0.4.0/lib/index.js"></script>
height: 500.0
history: True
margin: 0.0
maxScale: 1.0
minScale: 1.0
subtitle: Haskell Kernel
title: ThebeLab Test
width: 960
center: false
---
---

# ThebeLab Local Deck

## Start a local Jupyter kernel

``` {.make}
jupyter notebook \
  --NotebookApp.token=test-secret \
  --NotebookApp.allow_origin='https://localhost:8888/' \
  --NotebookApp.port=7777
```

# A ThebeLab Code Block {.columns}

## {.left grow=2}

``` {.haskell data-executable="true" data-language="haskell"}
print "Hallo!"
```
## Usage {.right}

1. Edit code
2. Click [run]
2. Rinse, repeat
