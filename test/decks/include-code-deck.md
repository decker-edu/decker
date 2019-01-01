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

``` {.haskell include="/src/Filter.hs" label="Haskell" snippet="includeCode"}
```
