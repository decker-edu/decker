---
title: New Quiz Syntax Test deck
history: True
# mario: True
---

# Overview


```
.quiz-match-items, .quiz-mi, .qmi {…}

.quiz-multiple-choice, .quiz-mc, .qmc {…}

.quiz-insert-choices, .quiz-ic, .qic {…}

.quiz-free-text, .quiz-ft, .qft {…}
```

IMHO, we can implement this. Hence, if you all agree I would ask Armin to go for it. Since many ideas also stem from Mario’s solution, it might be wise to cross-check what can be borrowed from his branch? Mario? Armin?

# Matching

All we are missing here are distractors, i.e. a way to have choices which are not to be dragged to some place. However, if we can decide on a special target symbol for the latter, we couls also include this. I propose ‚!'.

#

## Matching Questions {.quiz-mi} 

Drag the elements to create correct pairings


A
: drag to A
: test2

Haskell 
: ![](include/06-metal.png)

B
: drag to B

decker
: [decker](http://go.uniwue.de/decker)

C
: $\Leftarrow$ C

_-_
: $\Leftarrow$ C

!
: Distractor A

!
: Distractor B

``` {.yaml}
score: 5
category: CG
lectureId: cg1
topic: yeah
```

# Multiple Choice

## Multiple-Choice {.quiz-mc}

All choices get an optional comment to be potentially shown with the result. 


- [X] a
    - So ist das. 
    - test
- [ ] b
    - So nicht 
    - test2
    - test3
- [ ] c
    - So auch nicht

``` {.yaml}
score: 5
category: CG
lectureId: cg1
topic: yeah
```

# Audience Response System { .quiz }

Wer bekommt am Ende die Prinzessin?

- [ ] Donkey Kong 
    - Nein, der ist böse!
- [ ] Sponge Bob 
    - Nein, der lebt unter Wasser!
- [ ] Kleine A-Loch
    - Nein, den mag keiner!
- [X] Supermario
    - Klar!

# Blanktext/Inserting MC

Like multiple-choice, however, the choices are shown as drop-down lists embedded inside the text. Hence, beware of the whitespaces below. 

## Inserting Multiple-Choices in Text {.quiz-ic} 

    
Decker is a software built using  


- [ ] Scala
    - nein
- [X] Haskell
    - pandoc
- [ ] Java
    - Gut
- [ ] Ruby 
    - glück


and builds upon

- [ ] LaTeX Beamer

.

##

These questions now include multiple versions of correct answers with additional optional comments (as before). Hence the Syntax is slightly different in its compact representation. 

# Free Text

## Free Text {.quiz-ft} 


Das Ergebnis von $2*2=~?$ ist?

- 4
    - Die perfekte Lösung 
- vier
    - Auch ok 
- four
    - Deutscher Studiengang
- fier
    - RTL? 

# Free text alternative

However, to have only one syntax to remember I propose we also accept this alternative form below. It would also allow us to use the comments to have almost right/wrong answers which we comment to the users. All we need here is a way to specify cases and a default. Saying this, if we add context dependent reg expressions to the matching list, that would be great for the utility, i.e., everything which matches but which is not in the already given list before, see:


## Free Text {.quiz-ft} 


Das Ergebnis von $2*2=~?$ ist?


- [X] 4
    - Die perfekte Lösung 
- [X] vier
    - Auch ok 
- [X] four
    - Deutscher Studiengang
- [ ] fier
    - RTL, no not really?
- [ ] *
    - ganz falsch, so oder so?


# Minimal Matching

## Matching Question {.qmi}

Question

A
: A1
: A2

B
: B1

!
: C

``` {.yaml}
score: 5
category: CG
lectureId: cg1
topic: yeah
```

# Minimal Multiple Choice 

## Multiple Choice Question {.qmc}

Question text

- [ ] A
  - nope
- [X] B
  - yes

``` {.yaml}
score: 1
category: CG
lectureId: cg1
topic: elementary 
```

# Minimal Insert 

## Insert Choices Question {.qic}

- [X] A
  - of course
- [ ] B 
  - uhm ...

is the first letter in the ABC. The second one is

- [ ] B
  - yep

``` {.yaml}
score: 1
category: elementaryschool
lectureId: abc1
topic: The alphabet 
```

# Minimal FreeText 

## FreeText Question TL {.qft}

What's the first letter in the alphabet?

- A
  - yep
- B
  - nope

## {.qft}

What's the fourth letter?

- [ ] C
- [X] D