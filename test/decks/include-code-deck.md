---
history: True
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

``` {.haskell include="/src/Filter.hs" label="Haskell"}
```

# Include just a range

``` {.haskell include="/src/Filter.hs" label="Haskell" startLine="222" endLine="225"}
```

# Include a tagged snippet

## Tagged source

``` {.haskell}
-- start snippet include-start-end
emptyIsEnd :: Text -> Bool
emptyIsEnd = isSnippetTag "8<|" ""
-- end snippet include-start-end
```

## Inclusion

``` {.haskell include="/src/Text/Pandoc/Filter/IncludeCode.hs" label="Haskell" snippet="include-start-end"}
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

``` {.haskell include="/src/Text/Pandoc/Filter/IncludeCode.hs" label="Haskell" snippet="include-shorter"}
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

``` {.haskell include="/src/Text/Pandoc/Filter/IncludeCode.hs" label="Haskell" snippet="include-even-shorter"}
```
