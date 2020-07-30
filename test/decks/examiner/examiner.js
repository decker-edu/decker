console.log("Adding examiner load event handler.");
window.addEventListener("load", () => {
  let exams = document.querySelectorAll("div.exa-quest");
  console.log(exams);
  for (exam of exams) {
    let answer = exam.querySelector(".reveal div.exa-mc");
    let choices = answer.querySelectorAll(".reveal div.exa-mc div.choice");
    let button = exam.getElementsByTagName("button")[0];
    for (choice of choices) {
      let check = choice.querySelector("div.check-box");
      check.addEventListener("click", e => {
        if (!hasAttribute(answer, "solved")) {
          toggleAttribute(e.target, "checked");
        }
      });
    }
    console.log(button);
    button.addEventListener("click", e => {
      console.log(exam);
      setAttribute(answer, "solved", true);
    });
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
