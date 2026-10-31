# Questions (`*-quest.yaml`)

Each question is one YAML file. Decker builds a preview
`private/<path>-quest.html` per question and one catalog of all questions,
`public/exam-builder/questions.json`, for the exam builder.

```yaml {.decker-example file="recursion-quest.yaml"}
LectureId: functional       # lecture identifier
TopicId: recursion          # topic within the lecture
Title: Base case
Points: 1
Difficulty: Easy            # Easy | Medium | Hard
Comment: ""                 # may be empty, but must be present
Question: |                 # Markdown
  What does `take 3 (iterate (*2) 1)` return?
Answer:
  tag: MultipleChoice
  Choices:
    - Correct: true
      TheAnswer: "`[1,2,4]`"
    - Correct: false
      TheAnswer: "`[2,4,8]`"
```

- **Required:** `LectureId`, `TopicId`, `Title`, `Points`, `Question`,
  `Answer`, `Difficulty`, `Comment`. A missing field is a hard error.
- **Optional:** `Exam` (default `true`; `false` for polls and surveys),
  `ShuffleAnswers` (default `true`).
- **Answer types** (`tag` and its fields):

  | `tag`             | Fields                                                   |
  |-------------------|----------------------------------------------------------|
  | `MultipleChoice`  | `Choices: [{TheAnswer, Correct}]` (`Correct`: boolean)   |
  | `MultipleAnswers` | `WidthInMm`, `Answers: [{Detail, Correct}]` (both text)  |
  | `FillText`        | `FillText`, `CorrectWords` (list of text)                |
  | `FreeForm`        | `HeightInMm`, `CorrectAnswer`                            |
  | `Numerical`       | `CorrectNumber` (integer)                                |

````yaml {.decker-example file="essay-quest.yaml"}
LectureId: functional
TopicId: laziness
Title: Lazy evaluation
Points: 4
Difficulty: Medium
Comment: Graded by hand.
Question: |
  Explain why this terminates:

  ```haskell
  take 3 [1 ..]
  ```
Answer:
  tag: FreeForm
  HeightInMm: 40
  CorrectAnswer: Only the first three elements are evaluated.
````

## Gotchas

- YAML is **whitespace-sensitive**: fenced code inside `Question: |` must stay
  indented under the block scalar (as above).
- Quote answers that start with a YAML-special character (backtick, `*`, `[`,
  `{`, `:`), as in ``TheAnswer: "`[1,2,4]`"``.
- Field names are case-sensitive. An unknown field is silently ignored by the
  parser; the build reports it as `quest-unknown-field`:

```yaml {.decker-example file="typo-quest.yaml" expect="quest-unknown-field"}
LectureId: functional
TopicId: recursion
Title: Typo
Points: 1
Difficulty: Easy
Comment: ""
ShuffleAnswer: false
Question: Which one is a list?
Answer:
  tag: MultipleChoice
  Choices:
    - Correct: true
      TheAnswer: "`[1]`"
    - Correct: false
      TheAnswer: "`1`"
```

## Validation

- One question: `decker --strict private/<path>/<name>-quest.html` parses the
  YAML (a parse error is a hard error), checks the fields and renders the
  question.
- All questions: `decker --strict public/exam-builder/questions.json`. Adding
  or removing a question file is picked up without forcing a rebuild.
- Questions sharing `LectureId` and `TopicId` are randomized variants; only one
  is drawn per student exam. Keep variants equal in difficulty and number of
  choices.

See the [users' guide](https://decker-edu.github.io/decker/users-guide/decker-users-guide.html), section *Exam questions*.
