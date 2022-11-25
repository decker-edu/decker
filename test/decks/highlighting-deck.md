---
extra-highlight-syntax:
  Asm6502: asm6502.xml
highlight-style: monochrome
title: Pandoc Syntax Highlighting
---

# Custom Highlight Syntax Definition {.columns}

## For examlple: 6502 assembler {.left .accent2}

``` asm6502
Main:
  sei
  clc
  xce

  rep #$10        ;16 bit xy
  sep #$20        ; 8 bit a

  .dw $1C02,$1C02,$1C02,$1C02

lda sincos.l + $80,X
```

## Only works if {.right .accent3}

-   The name of the syntax in the XML file

    1.  is used as the key in `extra-highlight-syntax`
    2.  and as the language identifier on the code block

-   The name in `asm6502.xml`

    ``` xml
    <language name="Asm6502" ...
    ```
