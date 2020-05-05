---
title: New Quiz Syntax Test deck
history: True
---

# Overview


```
.quiz-match-items, .quiz-mi, .qmi {…}

.quiz-multiple-choice, .quiz-mc, .qmc {…}

.quiz-insert-choices, .quiz-ic, .qic {…}

.quiz-free-text, .quiz-ft, .qft {…}
```

# Matching

#

## test {.qmi}

Gravity
: $(0, -k\,m, 0)\T$

Damping
: $-k \, \dot{\vec{x}}$

Collisions
: $k \, d \, \vec{n}$

Inertia
: $m \, \ddot{\vec{x}}$

Springs
: $-k \, \left( \norm{\vec{x}_0 - \vec{x}_1} - L \right) \, \frac{ \vec{x}_0 - \vec{x}_1 }{\norm{\vec{x}_0 - \vec{x}_1}}$

#

## Matching Questions {.quiz-mi} 

Drag the elements to create correct pairings

A
: drag to A


Image 
: ![](include/06-metal.png)

B
: drag to B

decker
: [decker](http://go.uniwue.de/decker)

C
: $\Leftarrow$ C

Empty
: !

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
    - So nicht. 
    - Tooltip2
- [ ] c
    - So auch nicht

``` {.yaml}
score: 5
category: CG
lectureId: cg1
topic: yeah
```


# Blanktext/Inserting MC

Like multiple-choice, however, the choices are shown as drop-down lists embedded inside the text. Hence, beware of the whitespaces below. 

## Inserting Multiple-Choices in Text {.quiz-ic} 

    
Decker is a software built using  


- [ ] Scala
    - Unfortunately not.
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



# Free Text
These questions now include multiple versions of correct answers with additional optional comments (as before). Hence the Syntax is slightly different in its compact representation. 

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
