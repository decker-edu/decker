---
title: 'Title Page Test Deck'
subtitle: 'Testing the Template'
author: # 'John Doe'
  name: John Doe
  url: "https://example.com/john-doe"

affiliation: # 'Testaffiliation'
  institution: 'Testinstitute'
  url: 'https://example.com/institute-url'
  logo: '/test/decks/assets/dummy-square.png'

copyright: # 'Test Copyright'
  type: '(c)'
  url: 'https://example.com/copyright'

author-url: 'https://example.com/author-url'
affiliation-url: 'https://example.com/affiliation-url'
copyright-url: 'https://example.com/copyright-url'

authors:
    - name: 'Test Testington'
      affiliation: 'Department of Tests'
      name-url: 'https://example.com/testington'
      affiliation-url: 'https://example.com/test-dep'
      logo: '/test/decks/assets/dummy-long.png'
    - name: 'Jonathan Deer'
      affiliation: 'Test Institute for Testing'
      name-url: 'https://example.com/j-d'
      affiliation-url: 'https://example.com/inst-test'
      logo: '/test/decks/assets/dummy-high.png'

date: '01.01.1990'

teaser: # 'test/decks/assets/dummy-square.png'
  width: '128px'
  height: '128px'
  url: '/test/decks/assets/dummy-square.png'

logos:
    - '/test/decks/assets/dummy-square.png' # should be "normal"
    - '/test/decks/assets/dummy-long.png' # should be "normal"
    - '/test/decks/assets/dummy-high.png' # should be scaled down
    - '/test/decks/assets/dummy-big.png' # should be the same as square because it was scaled down
---

# First Slide

::: columns-1-1

![](./assets/dummy-square.png){width=128px height=128px}

![](./assets/dummy-long.png){width=256px height=128px}

![](./assets/dummy-big.png){width=256px height=256px}

![](./assets/dummy-high.png){width=128px height=256px}

:::