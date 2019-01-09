---
author: Armin Bernstetter
date: '30.10.2018'
title: 'Haskell Resources [(DRAFT)]{style="color:red;"}'
---

# Executive summary

WIP: This page provides links to resources for learning Haskell and getting information about Haskell-related tools, libraries etc used in Decker.

## General Haskell resources

### Books
- ["Get Programming with Haskell"](https://www.manning.com/books/get-programming-with-haskell): Great introductory book for programmers wanting to learn Haskell syntax and how to get started writing productive Haskell code without needing to learn how to program first.
- ["Haskell Programming from first principles"](http://haskellbook.com/): Very extensive book (> 1000 pages) starting from the very beginning of learning how to program using Haskell.
- ["Learn You a Haskell for Great Good"](http://learnyouahaskell.com/): Entertaining to read but slightly outdated. Not the best resource to actually learn writing code as it does not contain exercises.

### Other
- ["The Haskell Tool Stack"](https://docs.haskellstack.org/en/stable/README/): `stack` is the Haskell build tool used in Decker
- ["Hackage"](http://hackage.haskell.org/): The Haskell community's central package archive of open source software.
- ["Haddock"](https://www.haskell.org/haddock/): A Haskell documentation tool
- ["Hoogle"](https://www.haskell.org/hoogle/): Hoogle is a Haskell API search engine, which allows you to search many standard Haskell libraries by either function name, or by approximate type signature (also available as command line tool via Hackage)
- Haskell docsets are also available for [Dash](https://kapeli.com/dash) (macOS), [Zeal](https://zealdocs.org/) (Linux) or [Velocity](http://velocity.silverlakesoftware.com/) (Windows) 



## Libraries used in Decker (links to hackage documentation)

- Pandoc:
    - General Pandoc documentation page on hackage: http://hackage.haskell.org/package/pandoc
    - pandoc-types: http://hackage.haskell.org/package/pandoc-types
    Types for representing a structured document (`Block`, `Inline` etc), defining the pandoc data structure, functions for building and manipulating pandoc documents (`walkM`, ...).
- Shake: http://hackage.haskell.org/package/shake
- blaze-html: http://hackage.haskell.org/package/blaze-html

## Special Topics

### Lenses
- [A reddit comment providing a great "too long; didn't read" summary](https://www.reddit.com/r/haskell/comments/9ded97/is_learning_how_to_use_the_lens_library_worth_it/e5hf9ai)
- [A talk by John Wiegley providing a greater overview (youtube)](https://www.youtube.com/watch?v=QZy4Yml3LTY)
- [An in depth tutorial on lenses (schoolofhaskell.com)](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial)
- [The github repository of the "lens" package and the corresponding wiki](https://github.com/ekmett/lens/wiki)