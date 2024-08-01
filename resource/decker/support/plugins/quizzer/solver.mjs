export function solveFreeTextQuiz(replacers) {
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
    const number = input.dataset["number"];
    const choices = quiz.choices[Number.parseInt(number) - 1];
    for (const option of choices.options) {
      if (option.label === input.value && option.correct === true) {
        wrapper.classList.remove("wrong");
        wrapper.classList.add("correct");
        checkmark.classList.remove("fa-times");
        checkmark.classList.add("fa-check");
        break;
      }
    }
  }
}

export function solveAssignmentQuiz() {}

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
