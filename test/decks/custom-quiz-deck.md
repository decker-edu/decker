---
title: Custom Quizzes
---

# Quiz Buttons

<script>

class Answer {
  
  answer = "";
  reason = "";
  correct = false;

}

class Question {

  question = "";
  answers = [];

}

function createButton(text) {
  const button = document.createElement("button");
  button.classList.add("quiz-button");
  button.innerHTML = text;
  return button;
}

let selectedAnswer;
let dragged;

function makeAssignmentQuiz(Q, container) {
  const qdiv = document.createElement("div");
  const qp = document.createElement("p");
  qp.innerText = Q.question;
  qdiv.classList.add("question");
  qdiv.appendChild(qp);
  const adiv = document.createElement("div");
  adiv.classList.add("answers");
  const rdiv = document.createElement("div");
  rdiv.classList.add("reason");
  container.appendChild(qdiv);
  container.appendChild(adiv);
  container.appendChild(rdiv);

  const categories = [];

  const answerArea = document.createElement("div");
  const answerTitle = document.createElement("h4");
  answerTitle.innerText = "Answers";
  answerArea.appendChild(answerTitle);
  const answerBucket = document.createElement("button");
  answerBucket.classList.add("answer-bucket");
  answerArea.appendChild(answerBucket);

  for(const A of Q.answers) {
    categories.push(A.reason);

    const answer = document.createElement("button");
    const label = document.createElement("span");
    label.innerHTML = A.answer;
    answer.appendChild(label);
    answer.classList.add("answer");
    answer.draggable = true;

    answer.solution = A.reason;
    
    answerBucket.appendChild(answer);

    answerBucket.addEventListener("click", () => {
      if(selectedAnswer) {
        answerBucket.appendChild(selectedAnswer);
        selectedAnswer.classList.remove("selected");
        selectedAnswer = null;
      }
      event.stopPropagation();
    })

    answer.addEventListener("click", (event) => {
      if(selectedAnswer === answer) {
        selectedAnswer = null;
        answer.classList.remove("selected");
      } else {
        if(selectedAnswer) {
          selectedAnswer.classList.remove("selected");
        }
        selectedAnswer = answer;
        answer.classList.add("selected");
      }
      event.stopPropagation();
    });

    answer.addEventListener("dragstart", (event) => {
      dragged = event.target;
    });

    answer.addEventListener("dragend", (event) => {
      dragged = null;
    })
  }

  const uniques = categories.filter((value, index, array) => array.indexOf(value) === index);

  const categoryField = document.createElement("div");
  categoryField.classList.add("categories");
  adiv.appendChild(answerArea);
  adiv.appendChild(categoryField);

  for(const category of uniques) {
    const area = document.createElement("button");
    const title = document.createElement("span");
    title.innerText = category;
    area.classList.add("category");
    area.appendChild(title);
    categoryField.appendChild(area);

    area.solution = category;

    area.addEventListener("click", (event) => {
      if(selectedAnswer) {
        area.appendChild(selectedAnswer);
        selectedAnswer.classList.remove("selected");
        selectedAnswer = null;
      }
      event.stopPropagation();
    });

    area.addEventListener("drop", (event) => {
      event.preventDefault();
      console.log("drop");
      if(event.target.classList.contains("category")) {
        if(dragged) {
          event.target.appendChild(dragged);
          dragged = null;
        }
      }
    })
  }

  const solveButton = document.createElement("button");
  const solveLabel = document.createElement("label");
  solveLabel.innerText = "Check Solution";
  solveButton.appendChild(solveLabel);
  rdiv.appendChild(solveButton);
  solveButton.addEventListener("click", () => {
    const answers = container.querySelectorAll(".answer");
    for(const answer of answers) {
      answer.classList.remove("correct");
      answer.classList.remove("incorrect");
      if(answer.solution) {
        if(answer.parentElement.solution === answer.solution) {
          answer.classList.add("correct");
        } else {
          answer.classList.add("incorrect");
        }
      } else {
        if(answer.parentElement.solution) {
          answer.classList.add("incorrect");
        } else {
          answer.classList.add("correct");
        }
      }
    }
  });

}

function makeOptionQuiz(Q, container) {
  const qdiv = document.createElement("div");
  const qp = document.createElement("p");
  qp.innerText = Q.question;
  qdiv.classList.add("question");
  qdiv.appendChild(qp);
  const adiv = document.createElement("div");
  adiv.classList.add("answers");
  const rdiv = document.createElement("div");
  rdiv.classList.add("reason");
  container.appendChild(qdiv);
  container.appendChild(adiv);
  container.appendChild(rdiv);

  const select = document.createElement("select");

  adiv.appendChild(select);

  for(const A of Q.answers) {
    const option = document.createElement("option");
    option.innerHTML = A.answer;
    option.value = A.answer;
    option.reference = A;
    select.appendChild(option);
  }

  select.addEventListener("change", (event) => {
    select.classList.remove("correct");
    select.classList.remove("incorrect");
    const value = event.target.value;
    const options = select.querySelectorAll("option");
    for(const option of options) {
      if(option.value === value) {
        const A = option.reference;
        rdiv.innerText = A.reason;
        if(A.correct) {
          select.classList.add("correct");
        } else {
          select.classList.add("incorrect");
        }
        break;
      }
    }
  });
}

function makeButtonQuiz(Q, container) {
  const qdiv = document.createElement("div");
  const qp = document.createElement("p");
  qp.innerText = Q.question;
  qdiv.classList.add("question");
  qdiv.appendChild(qp);
  const adiv = document.createElement("div");
  adiv.classList.add("answers");
  const rdiv = document.createElement("div");
  rdiv.classList.add("reason");
  container.appendChild(qdiv);
  container.appendChild(adiv);
  container.appendChild(rdiv);
  for(const A of Q.answers) {
    const button = createButton(A.answer);
    button.addEventListener("click", (event) => {
      if(A.reason) {
        rdiv.innerText = A.reason;
      }
      if(A.correct) {
        button.classList.add("correct");
      } else {
        button.classList.add("incorrect");
      }
    })
    adiv.appendChild(button);
  }
}

function makeFreeTextQuiz(Q, container) {
  const qdiv = document.createElement("div");
  const qp = document.createElement("p");
  qp.innerText = Q.question;
  qdiv.classList.add("question");
  qdiv.appendChild(qp);
  const adiv = document.createElement("div");
  adiv.classList.add("answers");
  const rdiv = document.createElement("div");
  rdiv.classList.add("reason");
  container.appendChild(qdiv);
  container.appendChild(adiv);
  container.appendChild(rdiv);

  const input = document.createElement("input");
  input.type = "text";

  adiv.appendChild(input);

  input.addEventListener("keydown", (event) => {
    if(event.key === "Enter") {
      input.classList.remove("correct");
      input.classList.remove("incorrect");
      for(const A of Q.answers) {
        if(A.answer === input.value) {
          rdiv.innerHTML = A.reason;
          if(A.correct) {
            input.classList.add("correct");
          }
        }
      }
      if(!input.classList.contains("correct")) {
        input.classList.add("incorrect");
      }
    }
  })
}

function parseQuizzes() {
  const containers = document.querySelectorAll(".custom-quiz");
  for(const container of containers) {
    const question = container.querySelector("p");
    const lists = container.querySelectorAll("ul");
    for(list of lists) {
      const options = list.querySelectorAll(":scope > li");
      const Q = new Question();
      Q.question = question.innerText;
      for(const option of options) {
        const A = new Answer();
        const checkbox = option.querySelector("input");
        checkbox.remove();
        Q.answers.push(A);
        const reasonList = option.querySelector("ul");
        const reason = option.querySelector("li");
        A.reason = reason.innerText;
        reasonList.remove();
        A.answer = option.innerHTML;
        if(option.classList.contains("task-yes")) {
          A.correct = true;
        }
      }
      question.remove();
      list.remove();
      makeAssignmentQuiz(Q, container);
    }
  }
}

window.addEventListener("load", () => {
//  parseQuizzes();
})

</script>

<style>

.custom-quiz {
  text-align: center;
  border: 1px solid var(--foreground-color);
  border-radius: 0.25rem;
  padding: 0.5rem;
}

.custom-quiz .question {
  background-color: var(--accent5-bbg);
  border-radius: 0.5rem;
  border: 3px solid var(--accent5);
}

.custom-quiz .quiz-button {
  border: 2px solid var(--accent4);
  background-color: var(--accent4-bbg);
  padding: 2rem;
  font-size: 1rem;
}

.custom-quiz .quiz-button:hover {
  background-color: var(--accent4-bg);
}

.custom-quiz .quiz-button.correct {
  border: 2px solid var(--accent3);
  background-color: var(--accent3-bbg);
}

.custom-quiz .quiz-button.correct:hover {
  background-color: var(--accent3-bg);
}

.custom-quiz .quiz-button.incorrect {
  border: 2px solid var(--accent0);
  background-color: var(--accent0-bbg);
}

.custom-quiz .quiz-button.incorrect:hover {
  background-color: var(--accent0-bg);
}

.custom-quiz input.correct {
  border: 1px solid var(--accent3);
  background-color: var(--accent3-bg);
}

.custom-quiz input.incorrect {
  border: 1px solid var(--accent0);
  background-color: var(--accent0-bg);
}

.custom-quiz select.correct {
  border: 1px solid var(--accent3);
  background-color: var(--accent3-bg);
}

.custom-quiz select.incorrect {
  border: 1px solid var(--accent0);
  background-color: var(--accent0-bg);
}

.custom-quiz .categories {
  display: flex;
  flex-direction: row;
  width: 100%;
  justify-content: space-between;
}

.custom-quiz .category {
  min-width: 8rem;
  min-height: 8rem;
  margin: 1rem;
  border: 1px solid var(--foreground-color);
  border-radius: 0.25rem;
  display: flex;
  flex-direction: column;
  align-items: center;
  background-color: var(--shade1);
}

.custom-quiz .category:hover {
  background-color: var(--shade2);
}

.custom-quiz .answer-bucket {
  width: 100%;
  min-height: 4rem;
  border: 1px solid var(--foreground-color);
  border-radius: 0.25rem;
  background-color: var(--shade1);
}

.custom-quiz .answer-bucket:hover {
  background-color: var(--shade2);
}

.custom-quiz .answer {
  padding: 0.5rem;
  background-color: var(--shade1);
  border: 3px solid var(--blueish);
  border-radius: 0.5rem;
  cursor: pointer;
}

.custom-quiz button.answer.correct {
  border: 3px solid var(--accent3);
  background-color: var(--accent3-bbg);
}

.custom-quiz button.answer.incorrect {
  border: 3px solid var(--accent0);
  background-color: var(--accent0-bbg);
}

.custom-quiz button.answer.selected {
  border: 3px solid var(--accent6);
}

.custom-quiz button.answer:hover {
  background-color: var(--shade3);
}

</style>

::: {.quizzer .dnd}

This is a testquestion.

- [ ] A
  - Move A here
- [X] E = mc^2
  - Einstein
- [ ] C
  - Move C here

:::
