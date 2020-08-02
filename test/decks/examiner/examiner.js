window.addEventListener("load", () => {
  let exams = document.querySelectorAll("div.exa-quest");
  for (exam of exams) {
    console.log(exam);
    let button = exam.getElementsByTagName("button")[0];

    // Multiple choice answer
    let mcAnswer = exam.querySelector("div.exa-mc");
    if (mcAnswer !== null) {
      let choices = mcAnswer.querySelectorAll("div.exa-mc div.choice");
      for (choice of choices) {
        let check = choice.querySelector("div.check-box");
        check.addEventListener("click", e => {
          if (!hasAttribute(mcAnswer, "solved")) {
            toggleAttribute(e.target, "checked");
          }
        });
      }
      button.addEventListener("click", e => {
        setAttribute(mcAnswer, "solved", true);
        setAttribute(button, "solved", true);
      });
    }

    // Free form answer
    let ffAnswer = exam.querySelector("div.exa-ff");
    if (ffAnswer !== null) {
      let textarea = ffAnswer.querySelector("textarea");
      button.addEventListener("click", e => {
        setAttribute(ffAnswer, "solved", true);
        setAttribute(button, "solved", true);
        textarea.setAttribute("readonly", true);
      });
    }

    // Multiple answers
    let maAnswer = exam.querySelector("table.exa-ma");
    if (maAnswer !== null) {
      let rows = exam.querySelectorAll("table.exa-ma tr.detail");
      button.addEventListener("click", e => {
        setAttribute(maAnswer, "solved", true);
        setAttribute(button, "solved", true);
        for (row of rows) {
          let correct = getAttribute(row, "correct");
          let select = row.querySelector("select");
          if (select.value === correct) {
            setAttribute(row, "right", true);
          } else {
            setAttribute(row, "wrong", true);
          }
          select.setAttribute("disabled", true);
        }
      });
    }
  }
});

function getAttribute(e, attrName) {
  return e.getAttribute("data-" + attrName);
}

function hasAttribute(e, attrName) {
  return e.getAttribute("data-" + attrName) !== null;
}

function setAttribute(e, attrName, value) {
  e.setAttribute("data-" + attrName, value);
}

function toggleAttribute(e, attrName) {
  if (e.getAttribute("data-" + attrName) === null) {
    e.setAttribute("data-" + attrName, true);
  } else {
    e.removeAttribute("data-" + attrName);
  }
}

function toggleClass(e, className) {
  if (e.classList.contains(className)) {
    e.classList.remove(className);
  } else {
    e.classList.add(className);
  }
}
