---
lang: de-DE
title: Include Code
---

# Embedd source code

## Source

```` markdown
``` haskell
includeCode :: Pandoc -> Decker Pandoc
includeCode (Pandoc meta blocks) = do
  included <- doIO $ walkM (P.includeCode Nothing) blocks
  return $ Pandoc meta included
```
````

## Inclusion

``` haskell
includeCode :: Pandoc -> Decker Pandoc
includeCode (Pandoc meta blocks) = do
  included <- doIO $ walkM (P.includeCode Nothing) blocks
  return $ Pandoc meta included
```

# Include the entire file

## Source

``` markdown
![](/src/Text/Decker/Filter/Media.hs){.haskell .code}
```

## Inclusion

![](/src/Text/Decker/Filter/Media.hs){.haskell .code}

# Include a tagged snippet (Image syntax)

## Source

``` markdown
![](/src/Text/Decker/Filter/Media.hs#include-even-shorter){.haskell .code}
```

## Inclusion

![](/src/Text/Decker/Filter/Media.hs#include-even-shorter){.haskell .code}

--------------------------------------------------------------------------------

# Detent all the things

![](/src/Text/Decker/Filter/Media.hs#dedent-test){.haskell .code}

# Many small code blocks

Paragraph one

``` txt
Code block 1
```

This is code block 2

``` txt
Code block 2
```

Paragraph three

``` txt
Code block 3
```

Paragraph 4
