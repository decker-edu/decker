import Client from "./client.js";

import {
  solveAssignmentQuiz,
  solveFreeTextQuiz,
  solveSelection,
  solveSelectionQuiz,
} from "./solver.js";

import localization from "./localization.js";

const l10n = localization();

let selectedAnswer = null;
let draggedAnswer = null;
let dropTarget = null;

/**
 * Resets assignment quiz selections so it does not cross
 * slide boundries.
 */
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

/**
 * Creates the DOM elements necessary for any quiz.
 * @returns
 */
function createQuizContainer() {
  const container = document.createElement("div");
  container.className = "quizzer-container";

  const questionContainer = document.createElement("div");
  questionContainer.className = "question-container";

  const questionParagraph = document.createElement("div");
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

/**
 * Creates a pair of a span and input element linked together.
 * If the input changes, the span does so, too.
 * @param {*} number
 * @returns
 */
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

/**
 * Creates a select element with the given options.
 * @param {*} options
 * @returns
 */
function createSelectElement(options) {
  const select = document.createElement("select");

  const placeholder = document.createElement("option");
  placeholder.disabled = true;
  placeholder.selected = true;
  placeholder.innerText = l10n.pickMessage;
  placeholder.reason = l10n.pickReason;
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

  select.addEventListener("change", (event) => {
    solveSelection(select);
  });

  return [select, wrapper];
}

export default {
  /**
   * Render an assignment quiz:
   *
   * *------------------------*
   * |    [ [A] [B] [C] ]     |
   * *------------------------*
   * | [1]      [2]       [3] |
   * *------------------------*
   * @param {*} parent
   * @param {*} quiz
   * @returns
   */
  renderAssignmentQuiz(parent, quiz) {
    const container = createQuizContainer();
    container.question.innerHTML = quiz.question;
    if (quiz.choices.length !== 1) {
      container.classList.add("error");
      container.question.innerHTML = l10n.errorMultipleAssignments;
      parent.appendChild(container);
      container.removeChild(container.solutionContainer);
      return;
    }
    const choices = quiz.choices[0];

    const answerArea = document.createElement("fieldset");
    const answerLegend = document.createElement("legend");
    answerLegend.innerText = l10n.assignmentInstructionObjects;
    answerArea.classList.add("answer-fieldset");
    answerArea.appendChild(answerLegend);

    const answerBucket = document.createElement("button");
    answerBucket.classList.add("answer-bucket");
    answerBucket.classList.add("category");
    answerBucket.dataset["label"] = "unassigned";
    answerBucket.dataset["number"] = "0";

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

    const objectButtons = [];

    for (const option of choices.options) {
      const button = document.createElement("button");
      const label = document.createElement("span");
      label.innerHTML = option.label;
      button.appendChild(label);
      button.classList.add("answer");
      button.draggable = true;
      button.dataset["reason"] = option.reason || 0;
      button.answer = option;

      // Disallow dragging of images
      const images = button.querySelectorAll("img");
      for (const img of images) {
        img.draggable = false;
      }

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

      button.dataset["letter"] = option.letter;

      objectButtons.push(button);
      answerItems.appendChild(button);
    }

    const categoryArea = document.createElement("fieldset");
    const categoryLegend = document.createElement("legend");
    categoryArea.appendChild(categoryLegend);
    categoryLegend.innerText = l10n.assignmentInstructionCategories;
    categoryArea.classList.add("categories");
    container.answers.appendChild(categoryArea);

    const areas = [];
    for (const category of choices.categories) {
      const area = document.createElement("button");
      area.dataset["number"] = category.number;
      const label = document.createElement("span");
      label.innerHTML = `<span>${category.number}: </span>${category.label}`;
      area.dataset["label"] = category.label;
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
      solveAssignmentQuiz(objectButtons);
    };
    parent.appendChild(container);
  },

  /**
   *
   * @param {*} choices
   */
  renderSelectBox(choices) {
    const select = document.createElement("select");
    const placeholder = document.createElement("option");
    placeholder.disabled = true;
    placeholder.selected = true;
    placeholder.innerText = l10n.pickMessage;
    placeholder.reason = l10n.pickReason;
    placeholder.correct = false;
    select.appendChild(placeholder);
    const options = choices.options;
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

    select.addEventListener("change", (event) => {
      solveSelection(select);
    });
    return wrapper;
  },

  /**
   * Render a selection quiz:
   *
   * *------------------*
   * | Text [...v] Text |
   * |      [...v]      |
   * *------------------*
   * @param {*} parent
   * @param {*} quiz
   * @returns
   */
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
        const choices = quiz.choices[boxNumber];
        if (choices) {
          const options = quiz.choices[boxNumber].options;
          const [select, wrapper] = createSelectElement(options);
          selections.push(select);
          container.question.appendChild(wrapper);
          boxNumber++;
        } else {
          container.classList.add("error");
          container.question.innerHTML = l10n.errorMissingOptions;
          parent.appendChild(container);
          container.removeChild(container.solutionContainer);
          return;
        }
      } else {
        // If not, add the token as plain text.
        const span = document.createElement("span");
        let text = token.replace(/<p>/g, "");
        text = text.replace(/<\/p>/g, "");
        span.innerHTML = text;
        container.question.appendChild(span);
      }
    }
    for (; boxNumber < quiz.choices.length; boxNumber++) {
      const options = quiz.choices[boxNumber].options;
      const [select, wrapper] = createSelectElement(options);
      selections.push(select);
      container.answers.appendChild(wrapper);
    }
    container.solver.remove();
    parent.appendChild(container);
  },

  renderFreeTextInput(choices, number, placeholder) {
    const input = document.createElement("input");
    input.placeholder = placeholder ? placeholder : l10n.placeholder;
    input.placeholder = input.placeholder.replaceAll("{#}", number);
    const wrapper = document.createElement("wrapper");
    wrapper.classList.add("input-wrapper");
    wrapper.appendChild(input);
    input.addEventListener("keydown", (event) => {
      if (event.key !== "Enter") {
        return;
      }
      let checkmark = wrapper.querySelector(".checkmark");
      if (!checkmark) {
        checkmark = document.createElement("span");
        checkmark.classList.add("checkmark", "fas", "fa-times");
        wrapper.appendChild(checkmark);
      }
      wrapper.classList.add("wrong");
      checkmark.classList.add("fa-times");
      input.setAttribute("aria-description", l10n.wrong);
      for (const option of choices.options) {
        if (option.label === input.value) {
          if (option.correct === true) {
            wrapper.classList.remove("wrong");
            wrapper.classList.add("correct");
            checkmark.classList.remove("fa-times");
            checkmark.classList.add("fa-check");
            if (option.reason) {
              input.setAttribute(
                "aria-description",
                l10n.correct + " " + option.reason
              );
            } else {
              input.setAttribute("aria-description", l10n.correct);
            }
            break;
          } else if (option.reason) {
            input.setAttribute(
              "aria-description",
              l10n.wrong + " " + option.reason
            );
          }
        }
      }
    });
    return wrapper;
  },

  /**
   * Render a free text quiz:
   *
   * *-----------------*
   * | Text [...] Text |
   * |      [...]      |
   * *-----------------*
   *
   * @param {*} parent
   * @param {*} quiz
   * @returns
   */
  renderFreeTextQuiz(parent, quiz) {
    const container = createQuizContainer();

    const split = quiz.question.split(/(\[#[0-9]+\])/g);
    const replacers = [];

    let placeholderNumber = 1;
    let questionHTML = "";
    let placeholderSpans = [];
    for (const token of split) {
      if (token === "") {
        continue;
      }
      if (/\[#[0-9]+\]/.test(token)) {
        if (placeholderNumber > quiz.choices.length) {
          container.classList.add("error");
          container.question.innerHTML = l10n.errorMissingOptions;
          parent.appendChild(container);
          container.removeChild(container.solutionContainer);
          container.answers.remove();
          return;
        }
        const [span, replacer, wrapper] =
          createPlaceholderPair(placeholderNumber);
        container.answers.appendChild(wrapper);
        placeholderSpans.push(span);
        questionHTML =
          questionHTML + `<span data-number="${placeholderNumber}"></span>`;
        replacers.push(replacer);
        placeholderNumber++;
      } else {
        questionHTML = questionHTML + token;
      }
    }
    container.question.innerHTML = questionHTML;
    const placeholders =
      container.question.querySelectorAll("span[data-number]");
    for (let i = 0; i < placeholders.length; i++) {
      placeholders[i].replaceWith(placeholderSpans[i]);
    }
    for (; placeholderNumber - 1 < quiz.choices.length; placeholderNumber++) {
      const [span, replacer, wrapper] =
        createPlaceholderPair(placeholderNumber);
      container.answers.appendChild(wrapper);
      replacers.push(replacer);
    }
    container.solver.addEventListener("click", (event) => {
      solveFreeTextQuiz(replacers, quiz);
    });
    parent.appendChild(container);
  },

  /**
   *
   * @param {*} choices
   * @returns
   */
  renderChoiceButtons(choices) {
    const container = document.createElement("div");
    container.className = "answer-container";
    for (const option of choices.options) {
      const button = document.createElement("button");
      button.classList.add("answer");
      button.innerHTML = option.label;

      button.dataset["letter"] = option.letter;

      const popover = document.createElement("span");
      popover.className = "solution-popover";
      popover.setAttribute("aria-hidden", true);

      button.appendChild(popover);
      // The Popover API only allows positioning relative to the viewport right now.
      // Doing manual positioning and functionality via CSS right now but once the
      // Anchoring API is available this will be much easier.
      //      popover.popover = "manual";
      //      button.popoverTargetElement = popover;
      //      button.popoverTargetAction = "show";

      button.addEventListener(
        "click",
        (event) => {
          if (option.reason) {
            popover.innerHTML = option.reason;
          }
          if (option.correct) {
            button.classList.add("correct");
            const checkmark = document.createElement("span");
            checkmark.classList.add("checkmark", "fas", "fa-check");
            button.appendChild(checkmark);
            button.setAttribute(
              "aria-description",
              `${l10n.correct} ${option.reason ? option.reason : l10n.noReason}`
            );
          } else {
            button.classList.add("wrong");
            const checkmark = document.createElement("span");
            checkmark.classList.add("checkmark", "fas", "fa-times");
            button.appendChild(checkmark);
            button.setAttribute(
              "aria-description",
              `${l10n.wrong} ${option.reason ? option.reason : l10n.noReason}`
            );
          }
        },
        { once: true }
      );
      container.appendChild(button);
    }
    return container;
  },

  /**
   * Render a choice quiz:
   *
   * *-----------------*
   * |     Question    |
   * | [A]   [B]   [C] |
   * |    [D]   [E]    |
   * *-----------------*
   *
   * @param {*} parent
   * @param {*} quiz
   * @returns
   */
  renderChoiceQuiz(parent, quiz) {
    const container = createQuizContainer();
    if (quiz.question) {
      container.question.innerHTML = quiz.question;
    }
    if (quiz.choices.length > 1) {
      container.classList.add("error");
      container.question.innerText =
        "Malformed Quiz Data: Multiple answer sets for a multiple choice quiz.";
      parent.appendChild(container);
      container.removeChild(container.solutionContainer);
      return;
    }
    const answers = quiz.choices[0];

    if (!answers) {
      container.classList.add("error");
      container.question.innerText =
        "Malformed Quiz Data: No answers could be parsed.";
      parent.appendChild(container);
      container.removeChild(container.solutionContainer);
      return;
    }
    if (answers.parent && answers.parent !== parent) {
      container.answerContainer.appendChild(answers.parent);
    }
    for (const option of answers.options) {
      const button = document.createElement("button");
      button.classList.add("answer");
      button.innerHTML = option.label;

      button.dataset["letter"] = option.letter;

      button.setAttribute(
        "aria-description",
        container.questionContainer.textContent.trim()
      );

      const popover = document.createElement("span");
      popover.className = "solution-popover";
      popover.setAttribute("aria-hidden", true);

      button.appendChild(popover);
      button.popoverTargetElement = popover;

      button.addEventListener(
        "click",
        (event) => {
          if (option.reason) {
            popover.innerHTML = option.reason;
          }
          if (option.correct) {
            button.classList.add("correct");
            const checkmark = document.createElement("span");
            checkmark.classList.add("checkmark", "fas", "fa-check");
            button.appendChild(checkmark);
            button.setAttribute(
              "aria-description",
              `${l10n.correct} ${option.reason}`
            );
          } else {
            button.classList.add("wrong");
            const checkmark = document.createElement("span");
            checkmark.classList.add("checkmark", "fas", "fa-times");
            button.appendChild(checkmark);
            button.setAttribute(
              "aria-description",
              `${l10n.wrong} ${option.reason}`
            );
          }
        },
        { once: true }
      );
      if (answers.parent && answers.parent !== parent) {
        answers.parent.appendChild(button);
      } else {
        container.answers.appendChild(button);
      }
    }
    parent.appendChild(container);
  },
};
