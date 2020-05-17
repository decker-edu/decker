---
history: true
title: Include Code
---

# Inline the source code

``` {.haskell label="Haskell"}
includeCode :: Pandoc -> Decker Pandoc
includeCode (Pandoc meta blocks) = do
  included <- doIO $ walkM (P.includeCode Nothing) blocks
  return $ Pandoc meta included
```

# Include the entire file

``` {.haskell include="/src/Text/Decker/Filter/Filter.hs" label="Haskell"}
```

# Include just a range

``` {.haskell include="/src/Text/Decker/Filter/Filter.hs" label="Haskell" startLine="220" endLine="223"}
```

# Include a tagged snippet

## Tagged source

``` {.haskell}
-- start snippet include-start-end
isSnippetStart :: Text -> Text -> Bool
isSnippetStart name line =
  isSnippetTag "start snippet" name line ||
  isSnippetTag "8<|" name line || isSnippetTag "8<" name line
-- end snippet include-start-end
```

## Inclusion

``` {.haskell include="/src/Text/Decker/Filter/IncludeCode.hs" label="Haskell" snippet="include-start-end"}
```

# Include a tagged snippet (shorter)

## Tagged source

``` {.haskell}
-- 8< include-shorter
readIncluded :: Inclusion Text
readIncluded = liftIO . Text.readFile =<< asks include
-- >8
```

## Inclusion

``` {.haskell include="/src/Text/Decker/Filter/IncludeCode.hs" label="Haskell" snippet="include-shorter"}
```

# Include a tagged snippet (even shorter)

## Tagged source

``` {.haskell}
-- 8<| include-even-shorter
isSnippetTag :: Text -> Text -> Text -> Bool
isSnippetTag tag name line =
  mconcat [tag, " ", name] `Text.isSuffixOf` Text.strip line
```

## Inclusion

``` {.haskell include="/src/Text/Decker/Filter/IncludeCode.hs" label="Haskell" snippet="include-even-shorter"}
```

# Include a tagged snippet (Image syntax)

## Include source

``` {.markdown}
![](code:/src/Text/Decker/Filter/IncludeCode.hs#include-even-shorter){.haskell label="Haskell"}
```

## Inclusion

![](code:/src/Text/Decker/Filter/IncludeCode.hs#include-even-shorter){.haskell
label="Haskell"}

------------------------------------------------------------------------

# Many small code blocks

Paragraph one

``` {.txt}
Code block 1
```

This is code block 2

``` {.txt}
Code block 2
```

Paragraph three

``` {.txt}
Code block 3
```

Paragraph 4
