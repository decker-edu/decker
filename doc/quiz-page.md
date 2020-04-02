---
author: Armin Bernstetter
date: "02.04.2020"
---

# Quiz Syntax

## Class definition

For each question type ou can use either of the three tags to create quizzes

```
.quiz-match-items, .quiz-mi, .qmi

.quiz-multiple-choice, .quiz-mc, .qmc

.quiz-insert-choices, .quiz-ic, .qic 

.quiz-free-text, .quiz-ft, .qft
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

## Multiple Choice Question {.qmc}

Question text

- [ ] A
  - nope
- [X] B
  - yes

## Insert Choices Question {.qic}

- [X] A
  - of course
- [ ] B 
  - uhm ...

is the first letter in the ABC. The second one is

- [ ] B
  - yep

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

# Quiz Meta

Add a `YAML` code block to a question to provide meta information on the specific question.

````
``` {.yaml}
score: 5
category: FP
lectureId: fp1
topic: Functional Programming Introduction
```
````

# Quiz Html

The quizzes result in the following example `HTML` structure

## Matching Questions

```
    <div class="box columns qmi">
        <h2>Matching Question</h2>
        <p>Question text</p>
        <div class="buckets">
            <div class="bucket" data-bucketid="1">
                BucketA
            </div>
            <div class="bucket" data-bucketid="2">
                BucketB
            </div>
        </div>
        <div class="matchItems">
            <div class="matchItem" draggable="true" data-bucketid="1">
                A1
            </div>
            <div class="matchItem" draggable="true" data-bucketid="2">
                B1
            </div>
            <div class="matchItem distractor" draggable="true">
                Distractor
            </div>
            <div class="matchItem" draggable="true" data-bucketid="1">
                A2
            </div>
        </div>
        <br><button class="solutionButton">Show Solution</button>
    </div>

```

## Multiple Choice

```
    <div class="box columns qmc">
        <h2>Multiple Choice Question</h2>
        <p>Question text</p>
        <ul class="choices">
            <li class="wrong">
                A
                <div class="tooltip">nope</div>
            </li>
            <li class="correct">
                B
                <div class="tooltip">yes</div>
            </li><br>
        </ul>
        <br>
        <button class="solutionButton">Show Solution</button>
    </div>
```

## InsertChoices

```
    <div class="box columns qic">
        <h2>Insert Choices Question</h2>
        <select>
            <option class="correct">A</option>
            <option class="wrong">B</option>
        </select>
        is the first letter in the ABC. The second one is <input>
        <br><button class="solutionButton">Show Solution</button>
    </div>
```

## FreeText

```
    <div class="box columns qft">
        <h2>FreeText Question TL</h2>
        <p>What’s the first letter in the alphabet?</p>
        <input>
        <br>
        <button class="solutionButton">Show Solution</button>
    </div>

    <div class="box columns qft">
        <h2></h2>
        <p>What’s the fourth letter?</p>
        <input>
        <br>
        <button class="solutionButton">Show Solution</button>
    </div>
```