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
    // quizzes();
  }
});

function quizzes() {
  matchings();
  surveys();
  answerButton();
}

function matchings() {
  var dropzones = document.getElementsByClassName("dropzone");
  var draggables = document.getElementsByClassName("draggable");

  for (i = 0; i < dropzones.length; i++) {
    dropzones[i].addEventListener("drop", drop);
    dropzones[i].addEventListener("dragover", allowDrop);
  }

  for (i = 0; i < draggables.length; i++) {
    draggables[i].id = "drag".concat(i.toString());
    draggables[i].addEventListener("dragstart", drag);
  }
  // for (let drag of draggables) {
  // drag.addEventListener("ondragstart", drag);
  // }


}
// Functions for the matching drag/drop questions
// include into decker.js

function allowDrop(ev) {
  ev.preventDefault();
}

function drag(ev) {
  ev.dataTransfer.setData("text", ev.target.id);
}

function drop(ev) {
  ev.preventDefault();
  var data = ev.dataTransfer.getData("text");
  //ev.target.removeChild(ev.target.childNodes[0]);
  ev.target.appendChild(document.getElementById(data));
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
Provides the functionality for the buttons of free text questions
*/
function answerButton() {
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