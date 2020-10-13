---
title: Decker Quiz Overview
---

# Introduction

- This slide deck shows how to create simple quizzes with different question types for a self-learning scenario.
- Currently supported: 
  - "Fill-in-the-blank" questions/Cloze tests
  - Matching/pairing questions
  - Multiple choice questions
  - Freetext questions


# Quiz Syntax

## Class definition

For each question type you can use either of the three tags to create quizzes

```
.quiz-match-items, .quiz-mi, .qmi

.quiz-multiple-choice, .quiz-mc, .qmc

.quiz-insert-choices, .quiz-ic, .qic 

.quiz-free-text, .quiz-ft, .qft
```

# Basic syntax

Questions are defined by level 2 headers. That means creating a question **needs**

```
## Question title {.qmc}
```

(where `.qmc` can be replaced by any of the other quiz classes)


The quiz syntax (apart from matching questions) is based on the markdown task list syntax. A markdown task list looks like this

```
- [ ] This box is not checked
- [X] This box is checked
- [ ] Another unchecked box
```

You can add tooltips by creating a nested list e.g.

```
- [ ] A
  - tooltip A
- [X] B
  - tooltip B
```

# Fenced Divs Syntax

Alternatively, quizzes can be defined using the **fenced divs** syntax:

```
::: qmc
- [ ] A
  - tooltip A
- [X] B
  - tooltip B
:::
```

# Quiz Meta

Add a `YAML` code block below a question to provide meta information on the specific question.

- This is work in progress. Currently apart from `lang: de` or `lang: en` it does not do anything. (21. Jul 2020)
- If you put `lang: de` in the header of your slide deck, the static quiz content (e.g. buttons) will appear with german text.
- If you put `lang: de` only in the yaml block of a single question, only this question will be localized.

````
``` {.yaml}
lang: de
score: 5
category: FP
lectureId: fp1
topic: Functional Programming Introduction
```
````

# Matching Questions

These questions generate quizzes where a user can drag and drop items to sort them into "buckets".

This uses the Pandoc [definition list syntax](https://pandoc.org/MANUAL.html#definition-lists).

You can provide distractor items (items not belonging to any bucket) or empty buckets (no item belonging in those empty buckets) by using the exclamation mark "!".

# Matching Questions {.sub}

```
## Matching Question {.qmi}

Question text

BucketA
: A1
: A2

BucketB
: B1

!
: Distractor

Empty Bucket
: !
```

# Matching Questions Example

## Matching Question {.qmi}

Question text

BucketA
: A1
: A2

BucketB
: B1

!
: Distractor

Empty Bucket
: !

# Multiple Choice Questions

Classic multiple choice questions

```
## Multiple Choice Question {.qmc}

Which of these letters is the second in the alphabet?

- [ ] A
  - nope
- [X] B
  - yes
```

# Multiple Choice Questions Example {.sub}

## Multiple Choice Question {.qmc}

Question text

- [ ] A
  - nope
- [X] B
  - yes

# InsertChoices Questions

This will create a sort of blank text questions.
If multiple items are provided in the task list, they will be rendered as a drop down menu where the user can click answers.


```
## Insert Choices Question {.qic}

- [X] A
  - of course
- [ ] B 
  - uhm ...

is the first letter in the ABC. The second one is

- [X] B
  - yep
- [ ] C

```

# InsertChoices Questions Example {.sub}

## Insert Choices Question {.qic}

- [X] A
  - of course
- [ ] B 
  - uhm ...

is the first letter in the ABC. The second one is

- [X] B
  - yep
- [ ] C


# FreeText questions

This will create a simple input field/text box where the user can write their answer.

If there are wrong answers that are to be expected you can add those by not checking the task box. They will then show as wrong when clicking "Show Solution".

```
## FreeText Question TL {.qft}

What's the first letter in the alphabet?

- [X] A
  - yep
- [ ] B
  - nope

## {.qft}

What's the fourth letter?

- [ ] C
  - info
- [X] D

```

# FreeText Question Example without h2 title {.sub}

## {.qft}

What's the first letter in the alphabet?

- [X] A
  - yep
- [ ] B
  - nope

## {.qft}

What's the fourth letter?

- [ ] C
  - info
- [X] D

```yaml
lang: en
```


