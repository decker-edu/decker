---
title: Resource Runtime Variable Test

runtime-path-variables:
    - 'local-test-variable'

project-test-variable: '/test/decks/resources/test.png'
local-test-variable: '/test/decks/resources/test.png'
unused-test-variable: '/test/decks/resources/test.png'
---

# Resource Location Test

The meta-variable `project-test-variable` with the value `/test/decks/resources/test.png` should be converted to a relative filepath.

- variable value: [:meta](project-test-variable)

The meta-variable `local-test-variable` with the value `/test/decks/resources/test.png` should be converted to a relative filepath.

- variable value: [:meta](local-test-variable)

The meta-variable `unused-test-variable` is not on the `runtime-path-variable` list and should not be converted:

- variable value: [:meta](unused-test-variable)