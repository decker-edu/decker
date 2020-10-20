---
title: ThebeLab Test
subtitle: Haskell Kernel
width: 960.0
height: 500.0
margin: 0.0
maxScale: 1.0
minScale: 1.0
thebelab:
  enable: true
  binderUrl: 'https://mybinder.org'
  language: haskell
  repo: 'monofon/plc-notebooks'
  ref: master
  repoProvider: github
---

# ThebeLab Binder Deck

## ThebeLab

A Javascript client library for Jupyter servers that uses the Jupyter
API

-   [minrk/thebelab](https://github.com/minrk/thebelab)
-   [ThebeLab - ThebeLab
    documentation](https://thebelab.readthedocs.io/en/latest/)

## Binder

ThebeLab uses Binder to run the Jupyter server with the IHaskell kernel
from a custom Docker image on GitHub.

-   [The Binder Project](https://mybinder.org)
-   [gibiansky/IHaskell](https://github.com/gibiansky/IHaskell)
-   [monofon/plc-notebooks](https://github.com/monofon/plc-notebooks)

# A ThebeLab Code Block {.columns}

##  {.left grow="2"}

``` {.haskell data-executable="true" data-language="haskell"}
print "Was Gauss konnte, kann Haskell auch"

sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n-1)

sumtorial 100
```

## Usage {.right}

1.  Click \[run\]
2.  Be patient!
3.  Edit code
4.  Rinse, repeat
