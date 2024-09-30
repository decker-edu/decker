import Client from "./client.mjs";

import { solveFreeTextQuiz, solveSelectionQuiz } from "./solver.mjs";

import localization from "./localization.mjs";

const l10n = localization();

let selectedAnswer = null;
let draggedAnswer = null;
let dropTarget = null;

export function resetAssignmentState() {
  if (selectedAnswer) {
    selectedAnswer.classList.remove("selected");
  }
  selectedAnswer = null;
  draggedAnswer = null;
  dropTarget = null;
}

function handleDrop(event) {
  event.preventDefault();
  if (draggedAnswer && dropTarget) {
    dropTarget.appendChild(draggedAnswer);
  }
  draggedAnswer = null;
  dropTarget = null;
  return false;
}

function createQuizContainer() {
  const container = document.createElement("div");
  container.className = "quizzer-container";
  const questionContainer = document.createElement("div");
  questionContainer.className = "question-container";
  const questionParagraph = document.createElement("p");
  questionParagraph.className = "question-paragraph";
  questionContainer.appendChild(questionParagraph);
  const answerContainer = document.createElement("div");
  answerContainer.className = "answer-container";
  const solutionContainer = document.createElement("div");
  solutionContainer.className = "solution-container";
  const solveButton = document.createElement("button");
  solveButton.innerText = l10n.checkSolution;
  solveButton.type = "submit";
  solveButton.ariaLabel = l10n.checkSolution;
  solutionContainer.appendChild(solveButton);
  container.appendChild(questionContainer);
  container.questionContainer = questionContainer;
  container.question = questionParagraph;
  container.appendChild(answerContainer);
  container.answerContainer = answerContainer;
  container.answers = answerContainer;
  container.appendChild(solutionContainer);
  container.solutionContainer = solutionContainer;
  container.solver = solveButton;
  return container;
}

function createPlaceholderPair(number) {
  const span = document.createElement("span");
  const wrapper = document.createElement("div");

  const replacer = document.createElement("input");
  replacer.type = "text";
  replacer.placeholder = l10n.placeholder + number;
  replacer.addEventListener("keyup", (event) => {
    if (replacer.value !== "") {
      span.innerText = replacer.value;
    } else {
      span.innerText = `[${replacer.placeholder}]`;
    }
  });
  replacer.dataset["number"] = number;

  span.innerText = `[${replacer.placeholder}]`;

  wrapper.classList.add("input-wrapper");
  wrapper.appendChild(replacer);

  return [span, replacer, wrapper];
}

function createSelectElement(options) {
  const select = document.createElement("select");
  const placeholder = document.createElement("option");
  placeholder.disabled = true;
  placeholder.selected = true;
  placeholder.innerText = localization.pickMessage;
  placeholder.reason = localization.pickReason;
  placeholder.correct = false;
  select.appendChild(placeholder);
  for (const option of options) {
    const item = document.createElement("option");
    item.innerText = option.label;
    item.reason = option.reason;
    item.correct = option.correct;
    select.appendChild(item);
  }
  // Use an input-wrapper to align the checkmark.
  const wrapper = document.createElement("div");
  wrapper.classList.add("input-wrapper");
  wrapper.appendChild(select);

  const popover = document.createElement("div");
  popover.className = "solution-popover";
  wrapper.appendChild(popover);

  return [select, wrapper];
}

export default {
  renderAssignmentQuiz(parent, quiz) {
    const container = createQuizContainer();
    container.question.innerText = quiz.question;
    if (quiz.choices.length !== 1) {
      questionParagraph.innerText = localization.errorMultipleAssignments;
    }
    const choices = quiz.choices[0];
    const categories = [];

    const answerArea = document.createElement("fieldset");
    const answerLegend = document.createElement("legend");
    answerLegend.innerText = localization.assignemtInstructionObjects;
    answerArea.appendChild(answerLegend);

    const answerBucket = document.createElement("button");
    answerBucket.classList.add("answer-bucket");
    answerBucket.classList.add("category");
    answerBucket.dataset["label"] = "UNASSIGNED";
    const answerItems = document.createElement("div");
    answerItems.classList.add("items");
    answerBucket.appendChild(answerItems);
    answerBucket.addEventListener("click", (event) => {
      if (selectedAnswer) {
        answerItems.appendChild(selectedAnswer);
        selectedAnswer.classList.remove("selected");
        selectedAnswer = null;
      }
      event.stopPropagation();
    });

    let enterCounter = 0;

    function dragenter(event) {
      if (enterCounter === 0) {
        answerBucket.classList.add("dragover");
        dropTarget = answerItems;
      }
      enterCounter++;
      event.preventDefault();
    }

    function dragleave(event) {
      enterCounter--;
      if (enterCounter === 0) {
        answerBucket.classList.remove("dragover");
        dropTarget = null;
      }
    }

    function dragover(event) {
      event.preventDefault();
    }

    function drop(event) {
      enterCounter = 0;
      answerBucket.classList.remove("dragover");
    }

    const parts = [answerBucket, answerItems];

    for (const part of parts) {
      part.addEventListener("dragover", dragover);
      part.addEventListener("dragenter", dragenter);
      part.addEventListener("dragleave", dragleave);
      part.addEventListener("drop", handleDrop);
      part.addEventListener("drop", drop);
    }
    answerArea.appendChild(answerBucket);
    container.answers.appendChild(answerArea);

    const answers = [];

    for (const answer of choices.options) {
      if (answer.reason) {
        categories.push(answer.reason);
      }
      const button = document.createElement("button");
      const label = document.createElement("span");
      label.innerHTML = answer.label;
      button.appendChild(label);
      button.classList.add("answer");
      button.draggable = true;
      button.dataset["label"] = answer.label;
      button.dataset["reason"] = answer.reason;
      button.answer = answer;

      button.addEventListener("click", (event) => {
        if (selectedAnswer === button) {
          selectedAnswer = null;
          button.classList.remove("selected");
        } else {
          if (selectedAnswer) {
            selectedAnswer.classList.remove("selected");
          }
          selectedAnswer = button;
          button.classList.add("selected");
        }
        event.stopPropagation();
      });

      button.addEventListener("dragstart", (event) => {
        draggedAnswer = event.target;
      });

      button.addEventListener("dragend", (event) => {
        draggedAnswer = null;
      });

      answers.push(button);
    }

    function shuffle(array) {
      let current = array.length;
      while (current != 0) {
        let random = Math.floor(Math.random() * current);
        current--;
        [array[current], array[random]] = [array[random], array[current]];
      }
    }

    shuffle(answers);

    let letter = "A";
    for (const answer of answers) {
      answerItems.appendChild(answer);
      answer.dataset["letter"] = letter;
      answer.answer.letter = letter;
      letter = String.fromCharCode(letter.charCodeAt(0) + 1);
    }

    const uniqueCategories = categories.filter(
      (value, index, array) => array.indexOf(value) === index
    );

    const categoryArea = document.createElement("fieldset");
    const categoryLegend = document.createElement("legend");
    categoryArea.appendChild(categoryLegend);
    categoryLegend.innerText = localization.assignemntInstructionCategories;
    categoryArea.classList.add("categories");
    container.answers.appendChild(categoryArea);

    const areas = [];
    for (const category of uniqueCategories) {
      const area = document.createElement("button");
      const label = document.createElement("span");
      label.innerText = category;
      area.dataset["label"] = category;
      area.classList.add("category");
      area.appendChild(label);
      categoryArea.appendChild(area);
      const items = document.createElement("div");
      items.classList.add("items");
      area.appendChild(items);

      area.addEventListener("click", (event) => {
        if (selectedAnswer) {
          items.appendChild(selectedAnswer);
          selectedAnswer.classList.remove("selected");
          selectedAnswer = null;
        }
        event.stopPropagation();
      });

      let enterCounter = 0;

      function dragenter(event) {
        enterCounter++;
        if (enterCounter > 0) {
          area.classList.add("dragover");
          dropTarget = items;
        }
        event.preventDefault();
      }

      function dragleave(event) {
        enterCounter--;
        if (enterCounter === 0) {
          area.classList.remove("dragover");
          dropTarget = null;
        }
      }

      function dragover(event) {
        event.preventDefault();
      }

      function drop(event) {
        enterCounter = 0;
        area.classList.remove("dragover");
      }

      const parts = [area, label, items];

      for (const part of parts) {
        part.addEventListener("dragover", dragover);
        part.addEventListener("dragenter", dragenter);
        part.addEventListener("dragleave", dragleave);
        part.addEventListener("drop", handleDrop);
        part.addEventListener("drop", drop);
      }
      areas.push(area);
    }

    container.solver.onclick = () => {
      for (const answer of answers) {
        answer.classList.remove("correct");
        answer.classList.remove("wrong");
        let checkmark = answer.querySelector(".checkmark");
        if (!checkmark) {
          checkmark = document.createElement("span");
          checkmark.classList.add("checkmark", "fas");
          answer.appendChild(checkmark);
        }
        checkmark.classList.remove("fa-check");
        checkmark.classList.remove("fa-times");
        const category = answer.closest(".category").dataset["label"];
        const reason = answer.dataset["reason"];
        if (reason === "undefined") {
          if (category === "UNASSIGNED") {
            answer.classList.add("correct");
            checkmark.classList.add("fa-check");
          } else {
            answer.classList.add("wrong");
            checkmark.classList.add("fa-times");
          }
        } else if (category === reason) {
          answer.classList.add("correct");
          checkmark.classList.add("fa-check");
        } else {
          answer.classList.add("wrong");
          checkmark.classList.add("fa-times");
        }
      }
    };
    parent.appendChild(container);
  },

  renderSelectionQuiz(parent, quiz) {
    const selections = [];
    let boxNumber = 0;

    const container = createQuizContainer();

    const split = quiz.question.split(/(\[#[0-9]+\])/g);
    for (const token of split) {
      if (token === "") {
        continue; // Skip empty tokens.
      }
      if (/\[#[0-9]+\]/.test(token)) {
        // If it was a placeholder token, insert a select box.
        const options = quiz.choices[boxNumber++].options;
        const [select, wrapper] = createSelectElement(options);
        selections.push(select);
        container.question.appendChild(wrapper);
      } else {
        // If not, add the token as plain text.
        const span = document.createElement("span");
        span.innerText = token;
        container.question.appendChild(span);
      }
    }
    container.solver.addEventListener("click", (event) => {
      solveSelectionQuiz(selections);
    });
    parent.appendChild(container);
  },

  renderFreeTextQuiz(parent, quiz) {
    const container = createQuizContainer();

    if (!quiz.question) {
      container.classList.add("error");
      container.question.innerText = l10n.errorMissingQuestion;
      parent.appendChild(container);
      container.removeChild(container.solutionContainer);
      return;
    }

    const split = quiz.question.split(/(\[#[0-9]+\])/g);
    const replacers = [];

    let placeholderNumber = 1;
    for (const token of split) {
      if (token === "") {
        continue;
      }
      if (/\[#[0-9]+\]/.test(token)) {
        const [span, replacer, wrapper] =
          createPlaceholderPair(placeholderNumber);
        container.answers.appendChild(wrapper);
        container.question.appendChild(span);
        replacers.push(replacer);
        placeholderNumber++;
      } else {
        const span = document.createElement("span");
        span.innerText = token;
        container.question.appendChild(span);
      }
    }
    container.solver.addEventListener("click", (event) => {
      solveFreeTextQuiz(replacers);
    });
    parent.appendChild(container);
  },

  renderChoiceQuiz(parent, quiz) {
    const container = createQuizContainer();
    if (quiz.question) {
      container.question.innerText = quiz.question;
    } else {
      container.question.innerText = "Dieses Quiz besitzt keine Frage.";
    }
    if (quiz.choices.length > 1) {
      const error = document.createElement("p");
      error.innerText =
        "Malformed Quiz Data: Multiple answer sets for a multiple choice quiz.";
      return;
    }
    const answers = quiz.choices[0];
    for (const answer of answers.options) {
      const button = document.createElement("button");
      button.classList.add("answer");
      button.innerHTML = answer.label;
      const popover = document.createElement("span");
      popover.className = "solution-popover";
      popover.ariaLive = true;
      popover.role = "status";

      button.appendChild(popover);
      button.popoverTargetElement = popover;

      button.addEventListener(
        "click",
        (event) => {
          popover.innerHTML = answer.reason;
          if (answer.correct) {
            button.classList.add("correct");
            const checkmark = document.createElement("span");
            checkmark.classList.add("checkmark", "fas", "fa-check");
            button.appendChild(checkmark);
          } else {
            button.classList.add("wrong");
            const checkmark = document.createElement("span");
            checkmark.classList.add("checkmark", "fas", "fa-times");
            button.appendChild(checkmark);
          }
        },
        { once: true }
      );
      container.answers.appendChild(button);
    }
    parent.appendChild(container);
  },
};
