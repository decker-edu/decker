---
title: Resource Runtime Variable Test

runtime-path-variables:
    - 'resource-test-variable'

resource-test-variable: '/test/decks/resources/test.png'
local-test-variable: '/test/decks/resources/test.png'
---

# Resource Location Test

The meta-variable `resource-test-variable` with the value `/test/decks/resources/test.png` should be converted to a relative filepath.

- resource-test-variable: [:meta](resource-test-variable)

The meta-variable `local-test-variable` is not on the `runtime-path-variable` list and should not be converted:

- local-test-variable: [:meta](local-test-variable)