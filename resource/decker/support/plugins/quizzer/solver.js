import localization from "./localization.js";

const l10n = localization();

export function solveFreeTextQuiz(replacers, quiz) {
  for (const input of replacers) {
    const wrapper = input.closest(".input-wrapper");
    let checkmark = wrapper.querySelector(".checkmark");
    if (!checkmark) {
      checkmark = document.createElement("span");
      checkmark.classList.add("checkmark", "fas", "fa-times");
      wrapper.appendChild(checkmark);
    }
    wrapper.classList.add("wrong");
    checkmark.classList.add("fa-times");
    input.setAttribute("aria-description", l10n.wrong);
    const number = input.dataset["number"];
    const choices = quiz.choices[Number.parseInt(number) - 1];
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
  }
}

export function solveAssignmentQuiz(objectButtons) {
  for (const answer of objectButtons) {
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
    const category = answer.closest(".category").dataset["number"];
    const reason = answer.dataset["reason"];
    if (category === reason) {
      answer.classList.add("correct");
      checkmark.classList.add("fa-check");
    } else {
      answer.classList.add("wrong");
      checkmark.classList.add("fa-times");
    }
  }
}

export function solveSelection(selection) {
  const wrapper = selection.closest(".input-wrapper");
  let checkmark = wrapper.querySelector(".checkmark");
  if (!checkmark) {
    checkmark = document.createElement("span");
    checkmark.classList.add("checkmark", "fas", "fa-times");
    wrapper.appendChild(checkmark);
  }
  wrapper.classList.remove("correct");
  checkmark.classList.remove("fa-check");

  wrapper.classList.add("wrong");
  checkmark.classList.add("fa-times");

  const item = selection.options[selection.selectedIndex];
  const popover = wrapper.querySelector(".solution-popover");
  if (item.reason) {
    popover.hidden = false;
    popover.innerText = item.reason;
    selection.setAttribute(
      "aria-description",
      item.correct
        ? l10n.correct + " " + item.reason
        : l10n.wrong + " " + item.reason
    );
  } else {
    popover.hidden = true;
    selection.setAttribute(
      "aria-description",
      item.correct ? l10n.correct : l10n.wrong
    );
  }
  if (item.correct) {
    wrapper.classList.remove("wrong");
    wrapper.classList.add("correct");
    checkmark.classList.remove("fa-times");
    checkmark.classList.add("fa-check");
  }
}

export function solveSelectionQuiz(selections) {
  for (const selection of selections) {
    const wrapper = selection.closest(".input-wrapper");
    let checkmark = wrapper.querySelector(".checkmark");
    if (!checkmark) {
      checkmark = document.createElement("span");
      checkmark.classList.add("checkmark", "fas", "fa-times");
      wrapper.appendChild(checkmark);
    }
    wrapper.classList.remove("correct");
    checkmark.classList.remove("fa-check");

    wrapper.classList.add("wrong");
    checkmark.classList.add("fa-times");

    const item = selection.options[selection.selectedIndex];
    const popover = wrapper.querySelector(".solution-popover");
    popover.innerText = item.reason;
    if (item.correct) {
      wrapper.classList.remove("wrong");
      wrapper.classList.add("correct");
      checkmark.classList.remove("fa-times");
      checkmark.classList.add("fa-check");
    }
  }
}
