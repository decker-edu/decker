# Quizzes

Quizzes are questions on a slide that the audience answers in the browser. They
**only work in decks**. In a handout and on a page, a quiz is a plain list with
the correct answers checked, so never put secret answers on a page.

Each question is a level 2 heading (it can be empty) or a fenced div with one of
these classes:

| Type            | Class  | Long forms                               |
|-----------------|--------|------------------------------------------|
| Multiple choice | `.qmc` | `.quiz-mc`, `.quiz-multiple-choice`      |
| Matching        | `.qmi` | `.quiz-mi`, `.quiz-match-items`          |
| Insert choices  | `.qic` | `.quiz-ic`, `.quiz-insert-choices`       |
| Free text       | `.qft` | `.quiz-ft`, `.quiz-free-text`            |

A question needs a level 1 heading above it, or the slide breaks without a
diagnostic. A task list holds the answers. A nested list is the tooltip of an answer.

````markdown {.decker-example file="mc-deck.md"}
# Multiple choice

## Which is right? {.qmc}

Some text that belongs to the question.

- [ ] Wrong
  - The tooltip of this answer.
- [X] Correct
  - Another tooltip.

# As a div

::: qmc
- [ ] Wrong
- [X] Correct
:::
````

**Matching** uses a definition list: terms are buckets, definitions their items.
`!` as a term is a distractor, `!` as a definition an empty bucket.

````markdown {.decker-example file="match-deck.md"}
# Matching

## Sort these {.qmi}

Bucket A
: Item 1
: Item 2

Bucket B
: Item 3

!
: A distractor

Empty bucket
: !
````

**Insert choices** puts one selection box per task list into the text. The lists
must be separated by text, or Pandoc makes one list of them.

````markdown {.decker-example file="insert-deck.md"}
# Insert choices

## {.qic}

- [X] Haskell
  - Because of Pandoc.
- [ ] Java

is what Decker is written in. It builds upon

- [X] Pandoc
- [ ] PowerPoint

.
````

**Free text** compares the input, ignoring case and surrounding space. In a plain
list **every item is correct**, a common mistake. To comment on a wrong answer,
use a task list. An answer that is not listed is wrong.

````markdown {.decker-example file="free-deck.md"}
# All correct

## {.qft}

Two times two?

- 4
- four

# With a wrong answer

## {.qft}

What is the fourth letter?

- [ ] C
  - One too early.
- [X] D
````

## Style and options

The class `.plain` (or `.fancy`, the default) chooses the style of one question,
`quiz.style` that of a document or project. `.inline` puts the answers of a plain
multiple choice question in a row.

A YAML code block in a question is not shown. It sets the options of this question,
which are `lang` and `quiz.style`. The labels come from the dictionary in the
language of `lang`.

````markdown {.decker-example file="style-deck.md"}
---
quiz:
  style: plain
---

# Plain by default

## {.qmc .inline}

- [X] 4
- [ ] 5

# Options of one question

## {.qft}

``` yaml
lang: de
quiz:
  style: fancy
```

Which letter comes first?

- A
````

## Live quizzes

The whole audience answers on their phones, and the result appears on the slide.
This needs a server, so **do not use it unless the project has one**. Check
`decker info --json`: `config.decker-plugins.quizzer` must be on, and the deck needs
`quizzer.url`. A slide can only have one, and a second one replaces the slide with
an error message that the build does not report.

```yaml {.decker-example file="decker.yaml" project="live"}
decker-plugins:
  quizzer: true
quizzer:
  url: https://quiz.example.com/
```

The classes are `.quizzer-mc`, `.quizzer-ic`, `.quizzer-ft` and `.quizzer-mi`. A
multiple choice question without a correct answer is a survey. In `.quizzer-ic` and
`.quizzer-ft` the marks `[#1]`, `[#2]` place the fields, and a horizontal rule
separates the lists.

````markdown {.decker-example file="live-deck.md" project="live"}
# A survey

::: quizzer-mc
Which semester are you in?

- [ ] 1 to 2
- [ ] 3 to 4
:::

# Two fields

::: quizzer-ic
Decker is written in [#1] and uses [#2].

- [x] Haskell
- [ ] Java

---

- [x] Pandoc
- [ ] Word
:::

# Free text

::: {.quizzer-ft placeholder="Your answer"}
The result is [#1].

- [x] 4
- [x] four
:::
````

## Polls on exam questions

An exam question (`decker agent-guide quests`) in a deck can be polled in presenter
mode, if the project has a server in `poll-server`. Only `MultipleChoice` works.

```yaml {.decker-example file="decker.yaml" project="exam"}
poll-server: wss://polls.example.com/quiz
```

```yaml {.decker-example file="recursion-quest.yaml" project="exam"}
LectureId: functional
TopicId: recursion
Title: Base case
Points: 1
Difficulty: Easy
Comment: ""
Exam: false
Question: |
  Does every recursion need a base case?
Answer:
  tag: MultipleChoice
  Choices:
    - Correct: true
      TheAnswer: "Yes"
    - Correct: false
      TheAnswer: "No"
```

```markdown {.decker-example file="poll-deck.md" project="exam"}
# A question

![](recursion-quest.yaml){.question .poll}
```

Whiteboard, recording and everything else that happens in the running deck need
nothing in the source; see the
[users' guide](https://github.com/decker-edu/decker/blob/develop/users-guide/decker-users-guide.md),
sections *Quizzes*, *Live quizzes* and *During a presentation*.
