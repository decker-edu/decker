---
pandoc:
  filters:
    before:
    - upcase-h1.lua
subtitle: In Lua that is
title: Plugins
---

# A Lua Plugin {.columns}

## Meta data {.left}

``` yaml
pandoc:
 filters:
  before:
  - upcase-h1.lua
```

## `upcase-h1.lua` {.right grow=2}

``` lua
local text = pandoc.text

function Header(el)
  if el.level == 1 then
    return el:walk {
      Str = function(el)
        return pandoc.Str(
                 text.upper(el.text))
      end
    }
  end
end
```
