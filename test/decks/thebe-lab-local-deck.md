---
center: False
height: 540.0
margin: '0.0'
maxScale: 1.0
minScale: 1.0
subtitle: Haskell Kernel
thebelab:
  baseUrl: 'http://localhost:8192/'
  binderUrl: 'https://mybinder.org'
  enable: True
  language: haskell
  local: True
  ref: master
  repo: 'monofon/plc-notebooks'
  repoProvider: github
  token: plc
title: ThebeLab Test
width: 960.0
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

##  {.left grow="2"}

``` {.haskell data-executable="true"}
print "Hallo!"
```

## Usage {.right}

1.  Edit code
2.  Click \[run\]
3.  Rinse, repeat

# Multiple cells

``` {.haskell data-executable="true"}
add :: Num a => a -> a -> a
add a b = a + b
```

``` {.haskell data-executable="true"}
add 1 2
```

# 

``` {.haskell data-executable="true"}
sub :: Num a => a -> a -> a
sub a b = a - b
```