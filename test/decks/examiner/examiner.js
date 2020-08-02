function gradeMC(exam) {
  let mcAnswer = exam.querySelector("div.exa-mc");
  if (mcAnswer !== null) {
    let choices = mcAnswer.querySelectorAll("div.exa-mc div.choice");
    let nChoices = choices.length;
    let nChecks = 0;
    let nCorrectChoices = 0;
    let nCorrect = 0;
    let nWrong = 0;
    for (choice of choices) {
      let check = choice.querySelector("div.check-box");
      let checkIsCorrect = choice.classList.contains("correct");
      let isChecked = hasAttribute(check, "checked");

      if (checkIsCorrect) {
        nCorrectChoices += 1;
      }
      if (isChecked) {
        nChecks += 1;
      }
      if (checkIsCorrect && isChecked) {
        nCorrect += 1;
      }
      if (!checkIsCorrect && isChecked) {
        nWrong += 1;
      }
    }

    let maxPoints = getAttribute(exam, "points");
    let points = (
      (Math.max(0, nCorrect - nWrong) / nCorrectChoices) *
      parseInt(maxPoints)
    ).toFixed(2);

    let score = exam.querySelector("div.score");
    score.textContent += points + " / " + maxPoints;
  }
}

function gradeFF(exam) {
  let score = exam.querySelector("div.score");
  score.textContent += "0 / 0";
}

function gradeMA(exam) {
  let maAnswer = exam.querySelector("table.exa-ma");
  if (maAnswer !== null) {
    let details = maAnswer.querySelectorAll("tr.detail");
    let nDetails = details.length;
    let nCorrect = 0;
    let nWrong = 0;
    for (detail of details) {
      let select = detail.querySelector("select");
      let selectedOption = select.value;
      let correctOption = getAttribute(detail, "correct");

      if (selectedOption === correctOption) {
        nCorrect += 1;
      } else {
        nWrong += 1;
      }
    }

    let score = exam.querySelector("div.score");
    let maxPoints = getAttribute(exam, "points");

    let points = ((nCorrect / nDetails) * parseInt(maxPoints)).toFixed(2);

    score.textContent += points + " / " + maxPoints;
  }
}

window.addEventListener("load", () => {
  let exams = document.querySelectorAll("div.exa-quest");
  for (let exam of exams) {
    let button = exam.getElementsByTagName("button")[0];

    // Multiple choice answer
    let mcAnswer = exam.querySelector("div.exa-mc");
    if (mcAnswer !== null) {
      let choices = mcAnswer.querySelectorAll("div.exa-mc div.choice");
      for (choice of choices) {
        let check = choice.querySelector("div.check-box");
        check.addEventListener("click", e => {
          if (!hasAttribute(exam, "solved")) {
            toggleAttribute(e.target, "checked");
          }
        });
      }
      button.addEventListener("click", e => {
        gradeMC(exam);
        setAttribute(exam, "solved", true);
      });
    }

    // Free form answer
    let ffAnswer = exam.querySelector("div.exa-ff");
    if (ffAnswer !== null) {
      let textarea = ffAnswer.querySelector("textarea");
      button.addEventListener("click", e => {
        gradeFF(exam);
        setAttribute(exam, "solved", true);
        textarea.setAttribute("readonly", true);
      });
    }

    // Multiple answers
    let maAnswer = exam.querySelector("table.exa-ma");
    if (maAnswer !== null) {
      let rows = exam.querySelectorAll("table.exa-ma tr.detail");
      button.addEventListener("click", e => {
        gradeMA(exam);
        setAttribute(exam, "solved", true);
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
