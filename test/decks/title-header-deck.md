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

authors:
  - name: 'First Author'
    url: 'https://example.com/first'
    affiliation: # 'First Department'
      institution: 'First Department'
      url: 'https://example.com/first-dep'
      logo: '/test/decks/assets/dummy-long.png'
  - name: 'Second Author'
    url: 'https://example.com/second'
    affiliation: # 'Second Institute'
      institution: 'Second Institute'
#     url: 'https://example.com/second-dep'
      logo: '/test/decks/assets/dummy-high.png'

date: '01.01.1990'

teaser: # 'assets/dummy-square.png'
  width: '128px'
  height: '128px'
  url: 'assets/dummy-square.png'

logos:
    - '/test/decks/assets/dummy-square.png' # should be "normal"
    - '/test/decks/assets/dummy-long.png' # should be "normal"
    - '/test/decks/assets/dummy-high.png' # should be scaled down
    - '/test/decks/assets/dummy-big.png' # should be the same as square because it was scaled down
---

# Test

[:meta](logos)