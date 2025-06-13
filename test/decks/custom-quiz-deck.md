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

# Test

Rechne aus: $\frac{1}{3}$

::: quizzer-ft

- [x] 0,3
- [x] 0,33
- [x] 0,333

:::

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

# Choice Quiz as a poll with smaller size

::: { .quizzer .choice}

What is your semester?

::: {width=50%}

- [ ] `1 - 2`
- [ ] `3 - 4`
- [ ] `5 - 6`
- [ ] `>6`

:::

:::

# Choice Quiz with columns

::: { .quizzer .choice}

::: columns-1-1

::: col

What is your semester?

- [ ] `1 - 2`
- [ ] `3 - 4`
- [ ] `5 - 6`
- [ ] `>6`

:::

::: col

![](./@f{u}nn%20y.png)

:::

:::

:::

# Choice Quiz with Math

::: {.quizzer .choice}

What is $A \times B$?

- [x] $\begin{pmatrix}1 & 2 & 3\\4 & 5 & 6\\7 & 8 & 9\end{pmatrix}$
- [ ] $\begin{pmatrix}a & b & c\\d & e & f\\g & h & i\end{pmatrix}$
- [ ] $\begin{pmatrix}1 & 0 & 0\\0 & 1 & 0\\0 & 0 & 1\end{pmatrix}$

:::

# Selection Quiz - Placeholder Syntax

This syntax is currently unused in favour of directly copying the way the old syntax worked.

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

# Selection Quiz - Placeholder Syntax without Placeholders

This syntax is currently unused in favour of directly copying the way the old syntax worked.

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

# Selection Quiz - Old Syntax

::: {.quizzer .selection}

I ate 

- [x] Mensa

for dinner today.

:::

# Freetext Quiz - Old Syntax

::: {.quizzer .freetext placeholder="Custom Placeholder"}

I ate 

- [x] Mensa

for dinner today.

:::

# Freetext Quiz Multiple Fields

Multiple answer lines need to be separated with a horizontal rule.

::: {.quizzer .freetext}

What did you eat for dinner today?

- [x] Mensa

---

- [x] Pizza

:::

# Graceful Fail: Not enough Answers

As the placeholder syntax is currently not in use this has no impact right now.

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

# Assignment Quiz: Annoyingly long text

::: {.quizzer .assignment}

German is a crazy language

- [ ] Zeitgeberbausteingesteuerte Unterbrechungsroutine
  - Interrupt Handler
- [ ] Befestigung
  - Mountpoint
- [ ] Windhundverfahren
  - First-Come-First-Served

:::

# Assignment Quiz: Lots of options

::: {.quizzer .assignment}

- [ ] Zeitgeberbausteingesteuerte Unterbrechungsroutine
  - [Interrupt Handler]{.accent3}
- [ ] Befestigung
  - [Mountpoint]{.accent4}
- [ ] Windhundverfahren
  - [First-Come-First-Served]{.accent5}
- [ ] Adressraumbelegungsplan
  - Address Space Layout
- [ ] Störleitung
  - Interrupt Line
- [ ] Teilhaberbetrieb
  - Transaction Mode
- [ ] Flattern
  - Thrasing
:::

# Assignment Quiz: Lots of options manual adjustment

::: {style="font-size: 1.5rem;"}

::: {.quizzer .assignment}

- [ ] Zeitgeberbausteingesteuerte Unterbrechungsroutine
  - [Interrupt Handler]{.accent3}
- [ ] Befestigung
  - [Mountpoint]{.accent4}
- [ ] Windhundverfahren
  - [First-Come-First-Served]{.accent5}
- [ ] Adressraumbelegungsplan
  - Address Space Layout
- [ ] Störleitung
  - Interrupt Line
- [ ] Teilhaberbetrieb
  - Transaction Mode
- [ ] Flattern
  - Thrasing
:::

:::
# Assignment Quiz: Lots of options

::: quiz-mi

[Interrupt Handler]{.accent3}
: Zeitgeberbausteingesteuerte Unterbrechungsroutine

[Mountpoint]{.accent4}
: Befestigung

[First-Come-First-Served]{.accent5}
: Windhundverfahren

Address Space Layout
: Adressraumbelegungsplan

Interrupt Line
: Störleitung

Transaction Mode
: Teilhaberbetrieb

Thrasing
: Flattern

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

# Assignment Quiz with images

::: {.quizzer .assignment}

Assign these people to their correct group.

- [ ] ![](./@f{u}nn%20y.png){width="100px"}
  - A
- [ ] ![](./@f{u}nn%20y.png){width="100px"}
  - B
- [ ] ![](./@f{u}nn%20y.png){width="100px"}
  - C

:::

# Assignment Quiz via Definition List with images

::: {.quizzer .assignment}

Assign these people to their correct group.

Chancelor
: ![](./@f{u}nn%20y.png){width="100px"}

Professor
: ![](./@f{u}nn%20y.png){width="100px"}

Developer
: ![](./@f{u}nn%20y.png){width="100px"}

:::

# Special Case: Image as question text

::: {.quizzer .choice}

![](./@f{u}nn%20y.png)

- [x] Yes
- [ ] No

:::

# Special Case: vspace inside question text

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

# Questions in sub slides

::: {.quizzer .choice}

- [x] Yes
- [ ] No

:::

# Sub Slide {.sub}

::: {.quizzer .choice}

![](./@f{u}nn%20y.png)

- [x] Yes
- [ ] No

:::

# Block Behavior

## Test {.test}

Block Content

# Some special cases from the old quiz-deck.md test

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


# Test - old qic class

::: qic

This is some Text.

- [X] Pandoc
  - See previous tooltip.
- [ ] PowerPoint

::: test

This paragraph is inside a div, it is not removed and also not handled as quiz syntax.

- [X] Pandoc
  - See previous tooltip.
- [ ] PowerPoint

:::

![](./@f{u}nn%20y.png)

This is another paragraph.

:::

# Test - new quizzer-ic class

::: quizzer-ic

This is some Text.

- [X] Pandoc
  - See previous tooltip.
- [ ] PowerPoint

::: test

This paragraph is inside a div, it is not removed and also not handled as quiz syntax.

- [X] Pandoc
  - See previous tooltip.
- [ ] PowerPoint

:::

![](./@f{u}nn%20y.png)

This is another paragraph.

:::

# Trigger Tests

# .quizzer

::: quizzer

- [ ] One
  - Reason One (Wrong)
- [x] Two
  - Reason Two (Correct)
- [ ] Three
  - Reson Three (Wrong)

:::

# .quizzer.choice

::: {.quizzer .choice}

- [ ] One
  - Reason One (Wrong)
- [x] Two
  - Reason Two (Correct)
- [ ] Three
  - Reson Three (Wrong)

:::

# .quizzer-choice

::: quizzer-choice

- [ ] One
  - Reason One (Wrong)
- [x] Two
  - Reason Two (Correct)
- [ ] Three
  - Reson Three (Wrong)

:::

# .quizzer-mc

::: quizzer-mc

- [ ] One
  - Reason One (Wrong)
- [x] Two
  - Reason Two (Correct)
- [ ] Three
  - Reson Three (Wrong)

:::

# .quizzer-multiple-choice

::: quizzer-multiple-choice

- [ ] One
  - Reason One (Wrong)
- [x] Two
  - Reason Two (Correct)
- [ ] Three
  - Reson Three (Wrong)

:::

# .quizzer.selection

::: {.quizzer .selection}

- [ ] One
  - Reason One (Wrong)
- [x] Two
  - Reason Two (Correct)
- [ ] Three
  - Reson Three (Wrong)

:::

# .quizzer-selection

::: quizzer-selection

- [ ] One
  - Reason One (Wrong)
- [x] Two
  - Reason Two (Correct)
- [ ] Three
  - Reson Three (Wrong)

:::

# .quizzer-ic

::: quizzer-ic

- [ ] One
  - Reason One (Wrong)
- [x] Two
  - Reason Two (Correct)
- [ ] Three
  - Reson Three (Wrong)

:::

# .quizzer-item-choice

::: quizzer-item-choice

- [ ] One
  - Reason One (Wrong)
- [x] Two
  - Reason Two (Correct)
- [ ] Three
  - Reson Three (Wrong)

:::

# .quizzer.freetext

::: {.quizzer .freetext}

- [ ] One
  - Reason One (Wrong)
- [x] Two
  - Reason Two (Correct)
- [ ] Three
  - Reson Three (Wrong)

:::

# .quizzer-freetext

::: quizzer-freetext

- [ ] One
  - Reason One (Wrong)
- [x] Two
  - Reason Two (Correct)
- [ ] Three
  - Reson Three (Wrong)

:::

# .quizzer-free-text

::: quizzer-free-text

- [ ] One
  - Reason One (Wrong)
- [x] Two
  - Reason Two (Correct)
- [ ] Three
  - Reson Three (Wrong)

:::

# .quizzer-ft

::: quizzer-ft

- [ ] One
  - Reason One (Wrong)
- [x] Two
  - Reason Two (Correct)
- [ ] Three
  - Reson Three (Wrong)

:::

# .quizzer.assignment

::: {.quizzer .assignment}

- [ ] One
  - Reason One
- [x] Two
  - Reason Two
- [ ] Three
  - Reson Three

:::

# .quizzer-assignment

::: quizzer-assignment

- [ ] One
  - Reason One
- [x] Two
  - Reason Two
- [ ] Three
  - Reson Three

:::

# .quizzer-mi

::: quizzer-mi

- [ ] One
  - Reason One
- [x] Two
  - Reason Two
- [ ] Three
  - Reson Three

:::

# .quizzer-match-items

::: quizzer-match-items

- [ ] One
  - Reason One
- [x] Two
  - Reason Two
- [ ] Three
  - Reson Three

:::