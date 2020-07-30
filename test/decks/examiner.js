console.log("Adding examiner load event handler.");
window.addEventListener("load", () => {
  let exams = document.querySelectorAll("section.exa-mc");
  for (exam of exams) {
    let checks = exam.querySelectorAll("div.check-box");
    for (check of checks) {
      check.addEventListener("click", e => {
        toggleAttribute(e.target, "checked");
      });
    }
    let list = exam.querySelector("ul");
    let button = document.createElement("button");
    button.textContent = "Lösung anzeigen";
    button.addEventListener("click", e => {
      console.log(list);
      toggleAttribute(list, "solved");
    });
    exam.appendChild(button);
  }
});

function toggleAttribute(e, attrName) {
  if (e.getAttribute("data-" + attrName) == null) {
    e.setAttribute("data-" + attrName, true);
  } else {
    e.removeAttribute("data-" + attrName);
  }
}
