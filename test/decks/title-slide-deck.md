---
title: 'Title Slide Test Deck'
subtitle: 'Testing the Template'

# author: # 'John Doe'
#  name: 'John Doe'
#  url: 'https://example.com/john-doe'
#   affiliation: 'Author Affiliation'
#   name: 'Author Affiliation'
#   url: 'https://example.com/doe-corp'
#   logo: '/test/decks/assets/dummy-square.png'

affiliation: # 'Deck Affiliation'
  name: 'Deck Affiliation'
  url: 'https://example.com/deck-affiliation'
  logo: '/test/decks/assets/dummy-square.png'
#    uri: '/test/decks/assets/dummy-square.png'
#    width: '128px'
#    height: '128px'

copyright: # 'Test Copyright'
  type: '(c)'
  url: 'https://example.com/copyright'

authors:
  - name: 'First Author'
    url: 'https://example.com/first'
    affiliation: # 'First Department'
      name: 'First Department'
      url: 'https://example.com/first-dep'
      logo: '/test/decks/assets/dummy-long.png'
  - name: 'Second Author'
    url: 'https://example.com/second'
    affiliation: 'Second Department'
  - 'Third Author'

date: '01.01.1990'

# teaser: # 'assets/dummy-square.png'
#   width: '128px'
#   height: '128px'
#   uri: 'assets/dummy-square.png'

logos:
    - '/test/decks/assets/dummy-square.png' # should be "normal"
    - '/test/decks/assets/dummy-long.png' # should be "normal"
    - '/test/decks/assets/dummy-high.png' # should be scaled down
    - '/test/decks/assets/dummy-big.png' # should be the same as square because it was scaled down
    - uri: '/test/decks/assets/dummy-square.png' # should be "tiny"
      width: 16px
      height: 16px

showDeckerLink: false
# includeTULogo: false
---

# Title Meta

- `title`: [:meta](title)
- `subtitle`: [:meta](subtitle)

# Author Meta

- `author`: [:meta](author)
- `author.name`: [:meta](author.name)
- `author.url`: [:meta](author.url)
- `author.affiliation`: [:meta](author.affiliation)
- `author.affiliation.name`: [:meta](author.affiliation.name)
- `author.affiliation.url`: [:meta](author.affiliation.url)
- `author.affiliation.logo`: [:meta](author.affiliation.logo)
- `author.affiliation.logo.uri`: [:meta](author.affiliation.logo.uri)
- `author.affiliation.logo.width`: [:meta](author.affiliation.logo.width)
- `author.affiliation.logo.height`: [:meta](author.affiliation.logo.height)

# Affiliation Meta

- `affiliation`: [:meta](affiliation)
- `affiliation.name`: [:meta](affiliation.name)
- `affiliation.url`: [:meta](affiliation.url)
- `affiliation.logo`: [:meta](affiliation.logo)
- `affiliation.logo.uri`: [:meta](affiliation.logo.uri)
- `affiliation.logo.width`: [:meta](affiliation.logo.width)
- `affiliation.logo.height`: [:meta](affiliation.logo.height)

# Copyright Meta

- `copyright`: [:meta](copyright)
- `copyright.type`: [:meta](copyright.type)
- `copyright.url`: [:meta](copyright.url)

# Authors Meta

- `authors`: [:meta](authors)

# Date meta

- `date`: [:meta](date)

# Logo meta

- `logos`: [:meta](logos)
