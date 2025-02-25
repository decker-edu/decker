---
title: Custom Quizzes
quizzer:
  url: 'https://quiz.jetzt'
  audio:
    start: 'default'
    loop: 'default'
    end: 'default'
    volume: 0.75
reveal:
  center: true

---

# Choice Quiz

::: {.quizzer .choice}

What is **not** a mathematical operation?

- [ ] `(x + y)`
  - Wrong: This is simple addition.
- [ ] `(x - y)`
  - Wrong: This is simple subtraction.
- [x] `(x ^ y)`
  - Correct: This is bitwise XOR, a logical operation.
- [ ] `(x * y)`
  - Wrong: This is simple multiplication

:::

# Choice Quiz as a poll

::: { .quizzer .choice }

What is your semester?

- [ ] `1 - 2`
- [ ] `3 - 4`
- [ ] `5 - 6`
- [ ] `>6`

:::

# Choice Quiz as a poll

::: { .quizzer .choice }

What is your semester?

::: {width=50%}

- [ ] `1 - 2`
- [ ] `3 - 4`
- [ ] `5 - 6`
- [ ] `>6`

:::

:::

# Choice Quiz with Math

::: {.quizzer .choice}

What is $A \times B$?

- [x] $\begin{pmatrix}1 & 2 & 3\\4 & 5 & 6\\7 & 8 & 9\end{pmatrix}$
- [ ] $\begin{pmatrix}a & b & c\\d & e & f\\g & h & i\end{pmatrix}$
- [ ] $\begin{pmatrix}1 & 0 & 0\\0 & 1 & 0\\0 & 0 & 1\end{pmatrix}$

:::

# Select Quiz

::: {.quizzer .selection}

Complete this sentence:

The [#1] brown fox [#2] the lazy dog.

- [ ] fuzzy
  - No.
- [x] quick
  - Yes.
- [ ] energetic
  - No.

---

- [ ] targets
  - Ominous, but no.
- [ ] flattens
  - Foxes are not that heavy.
- [x] jumps over
  - Correct.

:::

# Select Quiz

::: {.quizzer .selection}

What attribute does the fox have and what does he do to the dog?

- [ ] fuzzy
  - No.
- [x] quick
  - Yes.
- [ ] energetic
  - No.

---

- [ ] targets
  - Ominous, but no.
- [ ] flattens
  - Foxes are not that heavy.
- [x] jumps over
  - Correct.

:::

# Freetext Quiz

::: {.quizzer .freetext}

I ate 

- [x] Mensa

for dinner today.

:::

# Freetext Quiz Multiple Fields

::: {.quizzer .freetext}

What did you eat for dinner today?

- [x] Mensa

---

- [x] Pizza

:::

# Graceful Fail: Not enough Answers

::: {.quizzer .freetext}

Test [#1] Two [#2] Three [#3]

- [ ] One
  - Reason One
- [x] Two
  - Reason Two
- [ ] Three
  - Reason Three

---

- [ ] What
  - That
- [ ] Where
  - There
- [x] When
  - Then

:::

# Graceful Fail: Multiple Quizzes

::: {.quizzer .freetext}

- [ ] One
  - Reason One
- [x] Two
  - Correct
- [ ] Three
  - Reson Three

:::

::: {.quizzer .selection}

- [ ] What
  - That
- [ ] Where
  - There
- [x] When
  - Correct

:::

# Assignment Quiz

::: {.quizzer .assignment}

Assign these people to their correct group.

- [ ] Olaf
  - Chancelor
- [ ] Mario
  - Professor
- [ ] Linus
  - Developer

:::

# Assignment Quiz via Definition List

::: {.quizzer .assignment}

Assign these people to their correct group.


Chancelor
: Olaf
: Bauer

Professor
: Mario

Developer
: Linus

:::

# Special Case: Image as question?

::: {.quizzer .choice}

![](./@f{u}nn%20y.png)

- [x] Yes
- [ ] No

:::

# Special Case: vspace inside question?

::: {.quizzer .choice}

Test

[:vspace](128px)

Test

- [x] Yes
- [ ] No

:::

# Old Selection Quiz Syntax

::: {.quizzer .selection}

Decker is a software built using  

- [ ] Scala
    - Unfortunately not.
    - A second tooltip.
- [X] Haskell
    - Due to Pandoc.
- [ ] Java
    - Surely not.
- [ ] Ruby 
    - hm no

and builds upon the tool

- [X] Pandoc
  - See previous tooltip.
- [ ] PowerPoint

.

:::

# Block Behavior

## Test {.test}

Block Content

# H2 QIC

## Hello World {.quizzer .selection}

This is some Text.

- [X] Pandoc
  - See previous tooltip.
- [ ] PowerPoint

Paragraph tags are removed from the entire question.

Hello World. This is a new paragraph.

![](./@f{u}nn%20y.png)

This is another paragraph.


# Test

::: qic

This is some Text.

- [X] Pandoc
  - See previous tooltip.
- [ ] PowerPoint

::: test

These paragraphs are inside a div, they are not removed.

- [X] Pandoc
  - See previous tooltip.
- [ ] PowerPoint

What is happending here?

:::

![](./@f{u}nn%20y.png)

This is another paragraph.

:::
