---
title: Decker Quiz Overview
history: true
---

# Introduction

- This slide deck shows how to create simple quizzes with different question types for a self-learning scenario.
- Currently supported: 
  - "Fill-in-the-blank" questions/Cloze tests
  - Matching/pairing questions
  - Multiple choice questions
  - Freetext questions

# Blank Text/Multiple Choice

```markdown
{blanktext} Optional Title
: Decker is a software built using the programming language 
{Scala|!Haskell|Java|Ruby} and builds upon 
{LaTeX Beamer|!RevealJS|PowerPoint}.

{blanktext}
: You {can't|!can} create blanktext questions without title
```
# Blank Text/Multiple Choice

{blanktext} Optional Title
: Decker is a software built using the programming language {Scala|!Haskell|Java|Ruby} and builds upon {LaTeX Beamer|!RevealJS|PowerPoint}.

{blanktext}
: You {can't|!can} create blanktext questions without title

# "Fill-in-the-blank" questions/Cloze tests
You can also add actual blank texts by providing only one option inside the curly brackets. Or combine both.

```markdown
{blanktext}
: These tests are called {Cloze} tests and are {!useful|useless}. 
```

{blanktext}
: These tests are called {Cloze} tests and are {!useful|useless}. 

# Matching Questions Syntax

- This type of questions asks to create pairs by dragging each element from a number of elements to the corresponding area.
- Currently only supports exact 1:1 pairing.

```markdown
{match} A
: pair with A

{match} Haskell
: ![](img/haskell.png)

...
```

# Matching Questions

{match} A
: pair with A

{match} Haskell 
: ![](img/haskell.png)

{match} B
: drag to B

{match} decker
: [decker](http://go.uniwue.de/decker)

{match} C
: $\Leftarrow$ C

# Freetext Questions Syntax

- Freetext questions consist of a bullet list of two elements with specific syntax
- Two separate questions have to be separated for example by using a level two header

```markdown
* {?} Question text
* {!} Correct solution

## 

* {?} Question 2
* {!} Answer

```

# Freetext Questions {layout="columns"}

## {.left} 
* {?} $2*2=~?$ 
* {!} 4

## 

* {?} The Answer to the Ultimate Question of Life, the Universe, and Everything is ...?
* {!} 42

## {.right}

* {?} Is this a question? 
* {!} yes

##

* {?} Name the capital of Germany
* {!} Berlin 

# Multiple Choice Questions Syntax

```markdown
* { } wrong answer
* { } another wrong answer
* {X} correct answer
* { } wrong answer again
```

# Multiple Choice Questions

## Question: Which file format does decker use? {.question}

* { } .docx
* { } .csv
* { } .xml
* {X} .md

