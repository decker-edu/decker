require('reveal.js/lib/js/head.min.js');
Reveal = require('reveal.js/js/reveal');
require('./decker.scss');
require('./fonts/roboto.css');
require('./fonts/source-code-pro.css');

window.addEventListener('ready', function (event) {
  if (Reveal.isReady()) {
    makeVertical();
    quizzes();
  } else {
    Reveal.addEventListener('ready', makeVertical);
  }
});

function quizzes() {
  matchings();
  surveys();
  freetextAnswerButton();
}

// Adds event listeners for dragging and dropping to the elements of "matching" questions
function matchings() {
  var dropzones = document.getElementsByClassName("dropzone");
  var draggables = document.getElementsByClassName("draggable");

  for (i = 0; i < dropzones.length; i++) {
    dropzones[i].id = "drop".concat(i.toString());
    dropzones[i].addEventListener("drop", drop);
    dropzones[i].addEventListener("dragover", allowDrop);

    for (let child of dropzones[i].children) {
      child.setAttribute("style", "pointer-events:none");
    }
  }

  for (i = 0; i < draggables.length; i++) {
    draggables[i].id = "drag".concat(i.toString());
    draggables[i].addEventListener("dragstart", drag);

    // disable children (e.g. images) from being dragged themselves
    for (let child of draggables[i].children) {
      child.setAttribute('draggable', false);
      child.className = "draggableChild";
    }
  }

  matchingAnswerButton();

}

/*
  Provides the functionality of the "show solution" button for matching questions
*/
function matchingAnswerButton() {
  var answerButtons = document.getElementsByClassName("matchingAnswerButton");

  for (let button of answerButtons) {
    button.onclick = function () {
      var matchingField = this.closest(".matching")
      var dropzones = matchingField.getElementsByClassName("dropzone");

      for (let drop of dropzones) {
        var draggables = drop.getElementsByClassName("draggable");
        if (draggables.length == 0) {
          return;
        }
      }
      for (let drop of dropzones) {
        var first = drop.getElementsByClassName("draggable")[0];
        if (first.id.replace("drag", "") == drop.id.replace("drop", "")) {
          drop.style.backgroundColor = "rgb(151, 255, 122)";
          first.setAttribute("draggable", false);
        }
        else {
          drop.style.backgroundColor = "rgb(255, 122, 122)";
          first.setAttribute("draggable", false);

        }
      }
    }

  }
}

// Functions for dragging and dropping in the matching questions 
function allowDrop(ev) {
  ev.preventDefault();
}

function drag(ev) {
  ev.dataTransfer.setData("text", ev.target.id);
}

function drop(ev) {
  ev.preventDefault();
  var data = ev.dataTransfer.getData("text");
  ev.target.appendChild(document.getElementById(data));
  ev.target.disabled = true;
}

/* simply copied from dachdecker/src-web/slide.js
to make the answer fields display red/green bg color even if not connected to dachdecker
*/
function surveys() {
  const surveys = document.getElementsByClassName("survey");
  let survey_num = 0;
  for (let survey of surveys) {
    survey.setAttribute("data-survey-num", survey_num);
    const local_survey_num = survey_num;
    survey_num += 1;
    const answers = survey.getElementsByTagName("li");
    let answer_num = 0;
    for (let answer of answers) {
      const local_answer_num = answer_num;
      answer.addEventListener("click", function () {
        const answer_div = this.getElementsByClassName("answer")[0];
        const is_right = answer_div.classList.contains("right");
        this.style.backgroundColor = (is_right) ? "#97ff7a" : "#ff7a7a";
        const tooltips = this.getElementsByClassName("tooltip");
        for (let tooltip of tooltips) {
          tooltip.style.display = "inline";
        }

      });
      answer_num += 1;
    }
  }

}

/*
Provides the functionality for the solution button of free text questions
*/
function freetextAnswerButton() {
  const answerButtons = document.getElementsByClassName('freetextAnswerButton');
  for (let button of answerButtons) {
    button.onclick = function () {
      var questionField = this.parentElement.getElementsByClassName('freetextInput')[0];
      if (questionField.value) {
        var answer = this.getElementsByClassName('freetextAnswer')[0];
        answer.style.display = 'block';
        if (questionField.value.toLowerCase().trim() == answer.textContent.trim().toLowerCase()) {
          questionField.style.backgroundColor = "rgb(151, 255, 122)";
        }
        else {
          questionField.style.backgroundColor = "rgb(255, 122, 122)";
        }
        questionField.disabled = 'true';
      }
    }
  }
}


window.printPdf = function () {
  url = window.location.href;
  url = url.replace(".html", ".html?print-pdf");
  var printWindow = window.open(url);


  printWindow.onload = function () {
    printWindow.print();
    printWindow.onfocus = function () { printWindow.close(); }
  };
}

function makeVertical() {
  const subsections = document.getElementsByClassName("sub");
  const subsection_bundles = [];
  for (let i = 0; i < subsections.length; i++) {
    const subsection = subsections[i];
    if (subsection.nodeName !== "SECTION") {
      continue;
    }
    const bundle = [subsection];
    while (
      i + 1 < subsections.length &&
      subsection.nextElementSibling === subsections[i + 1]
    ) {
      i += 1;
      bundle.push(subsections[i]);
    }
    subsection_bundles.push(bundle);
  }

  for (let bundle of subsection_bundles) {
    const supersection = document.createElement("section");
    supersection.classList.add("slide");
    supersection.classList.add("level1");
    const section = bundle[0].previousElementSibling;
    section.parentNode.insertBefore(supersection, section);
    supersection.appendChild(section);
    for (let subsection of bundle) {
      supersection.appendChild(subsection);
    }
  }
  Reveal.sync();
  Reveal.setState(Reveal.getState());
}