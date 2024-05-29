---
title: Custom Quizzes
quizzer:
  backend: 'http://localhost:3000'
  socket: 'ws://localhost:3000/api/websocket'
---

# Choice Quiz

::: {.quizzer .choice}

Question

- [ ] Wrong
  - Because I say so!
- [x] Correct
  - Because it is right!

:::

# Select Quiz

::: {.quizzer .selection}

[#1] Test [#2]

- [ ] One
  - Reason One
- [x] Two
  - Correct
- [ ] Three
  - Reson Three

---

- [ ] What
  - That
- [ ] Where
  - There
- [x] When
  - Correct

:::

# Freetext Quiz

::: {.quizzer .freetext}

[#1] Test [#2]

- [ ] One
- [x] Two
- [ ] Three

---

- [ ] What
- [ ] Where
- [x] When

:::

# Graceful Fail

::: {.quizzer .freetext}

- [ ] One
  - Reason One
- [x] Two
  - Correct
- [ ] Three
  - Reson Three

---

- [ ] What
  - That
- [ ] Where
  - There
- [x] When
  - Correct

:::

# Assignment Quiz

::: {.quizzer .assignment}

This is a testquestion.

- [ ] A
  - Move A here
- [ ] E = m * c hoch 2
  - Einstein
- [ ] C
  - Move C here
- [ ] Fake

:::
