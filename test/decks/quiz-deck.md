---
title: New Quiz Syntax Test deck
lang: de
---

# Overview
  
## Add quiz type to level 2 header: 

```.markdown
## Question 1 {.qmc}
```

## {.small}

- Multiple Choice: `.qmc, .quiz-mc, .quiz-multiple-choice`  
- Matching: `.qmi, .quiz-mi, .quiz-match-items`  
- Insert: `.qic, .quiz-ic, .quiz-insert-choices`  
- Free-text: `.qft, .quiz-ft, .quiz-free-text`  

## Styling

To switch all questions to plain style, add to metadata:

```.yaml
quiz:
  style: plain
```

Or to style individual questions, add `{.plain}`

# Multiple Choice

# Mutliple Choice 1 - Fancy

## {.quiz-mc}

All choices get an optional comment to be potentially shown with the result. 


- [X] a
    - So ist das. 
    - test
- [ ] b
    - So nicht. 
    - Tooltip2
- [ ] c
    - So auch nicht

```yaml
Points: 5
LectureId: cg1
TopicId: yeah
Difficulty: Easy
```

# Mutliple Choice 2 - Plain

## {.quiz-mc .plain}

Choices appear in a column. Click the circle to the left of the choice to select. Choices are corrected immediately. A green check will appear in the circle for correct responses, and a red x for incorrect responses. Hover over the circle to see a tooltip. All choices get an optional comment to be potentially shown with the result. 


- [X] a
    - So ist das. 
    - test
- [ ] b
    - So nicht. 
    - Tooltip2
- [ ] c
    - So auch nicht

```yaml
Points: 5
LectureId: cg1
TopicId: yeah
Difficulty: Easy
```

# Multiple Choice 3 - Fancy

## What is the result of the following term? {.qmc .x-small}

\begin{equation*}
\begin{bmatrix}
1&0&0&1\\
0&1&0&1\\
0&0&1&0\\
0&0&0&1
\end{bmatrix}
\begin{bmatrix}
0&-1&0&0\\
1&0&0&0\\
0&0&1&0\\
0&0&0&1
\end{bmatrix}
\end{equation*}

- [ ] $\begin{bmatrix}
0&1&0&1\\
-1&0&0&1\\
0&0&1&0\\
0&0&0&1
\end{bmatrix}$
   - As we will see, the second matrix describes a rotation. Combined with the first one, which describes a translation, the rotational part (upper left $3\times3$) would not be altered.
- [X] $\begin{bmatrix}
0&-1&0&1\\
1&0&0&1\\
0&0&1&0\\
0&0&0&1
\end{bmatrix}$
   - Yes, that is correct.
- [ ] $\begin{bmatrix}
0&-1&0&0\\
1&0&0&1\\
0&0&1&0\\
0&0&0&1
\end{bmatrix}$
   - As we will see, the first matrix describes a translattion. Combined with the second one, which describes a rotation, the translational part (right column) would not be altered.


```yaml
Points: 5
LectureId: cg1
TopicId: yeah
Difficulty: Easy
```

# Mutliple Choice 4 - Plain, Inline

Choices are displayed in a row. 

## What is the result of the following term? {.qmc .plain .inline .small }

\begin{equation*}
\begin{bmatrix}
1&0&0&1\\
0&1&0&1\\
0&0&1&0\\
0&0&0&1
\end{bmatrix}
\begin{bmatrix}
0&-1&0&0\\
1&0&0&0\\
0&0&1&0\\
0&0&0&1
\end{bmatrix}
\end{equation*}

- [ ] $\begin{bmatrix}
0&1&0&1\\
-1&0&0&1\\
0&0&1&0\\
0&0&0&1
\end{bmatrix}$
   - As we will see, the second matrix describes a rotation. Combined with the first one, which describes a translation, the rotational part (upper left $3\times3$) would not be altered.
- [X] $\begin{bmatrix}
0&-1&0&1\\
1&0&0&1\\
0&0&1&0\\
0&0&0&1
\end{bmatrix}$
   - Yes, that is correct.
- [ ] $\begin{bmatrix}
0&-1&0&0\\
1&0&0&1\\
0&0&1&0\\
0&0&0&1
\end{bmatrix}$
   - As we will see, the first matrix describes a translattion. Combined with the second one, which describes a rotation, the translational part (right column) would not be altered.


```yaml
Points: 5
LectureId: cg1
TopicId: yeah
Difficulty: Easy
```


# Matching

# Matching 1 - Fancy

## {.qmi}

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

```yaml
lang: en
```

# Matching 2 - Plain

Plain matchinq questions display a question/term, followed by a select box and a list of possible answers. Click the select box and then click answers as desired. Check marks will appear. Click outside of the select box to close. Select additional or deselect selected responses as desired. After clicking the solution button, answers will turn green or red to display correctness. Change answers as desired and click solve again.

A grey box surrounds the matching questions and responses to ensure that all are shown. The solution button is below this.

# Matching 2 - Plain

## {.qmi .plain}

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

```yaml
lang: en
```

# Matching 3 - Fancy

## {.quiz-mi} 

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

```yaml
score: 5
category: CG
lectureId: cg1
topic: yeah
```

# Matching 4 - Plain

## {.quiz-mi .plain} 

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

```yaml
score: 5
category: CG
lectureId: cg1
topic: yeah
```


# Matching 5 - Plain

## {.qmi .plain}

Geben Sie jeweils die verwendete, unter Umständen zusammengesetzte, Transformation für diese 5 Quadrate an.

Bild 1
: Rz(180)

Bild 2
: Sxy(1, -1)

Bild 3
: Sxy(3, 3)

Bild 4
: Sxy(0.5, 0.5) Txy(0.5, 0.5)

Bild 5
: Rz(-45)Txy(0.5, 0.5)

!
: Sxy(-1,1)

!
: Sxy(0.3, 0.3)

!
: Sxy(-1,1) Txy(0.5, 0.5)

!
: Rz(45)

Bild 6
: ! 

# Blanktext/Inserting MC

Like multiple-choice, however, the choices are shown as drop-down lists embedded inside the text. Hence, beware of the whitespaces below. 

# Insert 1 - Fancy

## {.quiz-ic} 

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

```yaml
Points: 5
```

# Insert 2 - Plain

Questions are corrected immediately, displaying red or green based on correctness. Retry is allowed. Hover over the select box to display the tooltip below the question.
 
## {.quiz-ic .plain} 

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

```yaml
Points: 5
```


# Free Text

These questions now include multiple versions of correct answers with additional optional comments (as before). Hence the Syntax is slightly different in its compact representation. 

# Free Text 1 - Fancy

## {.quiz-ft} 

Das Ergebnis von $2*2=~?$ ist?

- 4
    - Die perfekte Lösung 
- vier
    - Auch ok 
- four
    - Deutscher Studiengang
- fier
    - RTL? 

```yaml
Points: 5
```

# Free Text 2 - Plain

Type the answer and push Enter to correct the question.  

## {.qft .plain}

What's the first letter in the alphabet?

- [X] A
  - The modern form of the capital letter A evolved from a the Latin script, a transformation of Greek script, which is in turn a transformation of the Phoenician script.
- [ ] B
  - The letter B isn't used in any numbers until billion.

## {.qft .plain}

What's the fourth letter?

- [ ] C
    - "C" comes from the same letter as "G". The Semites named it gimel. The sign is possibly adapted from an Egyptian hieroglyph for a staff sling, which may have been the meaning of the name gimel. 
- [X] D
    - The letter "D" has retained the fourth place in the alphabet from the earliest point at which it appears in history. It corresponds to Semitic daleth and Greek delta (Δ). The form is thought to derive from an early pictograph, possibly Egyptian, indicating the folding door of a tent.

```yaml
Points: 5
```

# Free text alternative

This alternate forms allows you to define free-text questions as lists without brackets. All answers are correct.

# Free Text Alternative 1 - Fancy

## {.quiz-ft} 

Das Ergebnis von $2*2=~?$ ist?


- 4
    - Die perfekte Lösung 
- vier
    - Auch ok 
- four
    - Deutscher Studiengang
- fier
    - RTL, no not really?

```yaml
Points: 5
```

# Free Text Alternative 2 - Plain

## {.quiz-ft .plain} 

Das Ergebnis von $2*2=~?$ ist?

- 4
    - Die perfekte Lösung 
- vier
    - Auch ok 
- four
    - Deutscher Studiengang
- fier
    - RTL, no not really?

```yaml
Points:
```

# fenced divs syntax

::: qft

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
:::


#

::: qic

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

:::