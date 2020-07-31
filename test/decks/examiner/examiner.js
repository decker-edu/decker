console.log("Adding examiner load event handler.");
window.addEventListener("load", () => {
  let exams = document.querySelectorAll("div.exa-quest");
  console.log(exams);
  for (exam of exams) {
    let button = exam.getElementsByTagName("button")[0];
    // Multiple choice answer
    let mcAnswer = exam.querySelector(".reveal div.exa-mc");
    if (mcAnswer !== null) {
      let choices = mcAnswer.querySelectorAll(".reveal div.exa-mc div.choice");
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
        button.setAttribute("disabled");
      });
    }
    // Free form answer
    let ffAnswer = exam.querySelector(".reveal div.exa-ff");
    if (ffAnswer !== null) {
      let solution = ffAnswer.querySelector("div.correct");
      let textarea = ffAnswer.querySelector("textarea");
      button.addEventListener("click", e => {
        setAttribute(ffAnswer, "solved", true);
        button.setAttribute("disabled", true);
        textarea.setAttribute("readonly", true);
      });
    }
  }
});

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
