require('reveal.js/lib/js/head.min.js');
Reveal = require('reveal.js/js/reveal');
require('./decker.scss');
require('./fonts/roboto.css');
require('./fonts/source-code-pro.css');

window.addEventListener('ready', function (event) {
  fixAutoplayWithStart();
  if (Reveal.isReady()) {
    makeVertical();
    quizzes();
    currentDate();
  } else {
    Reveal.addEventListener('ready', makeVertical);
  }
});

function fixAutoplayWithStart() {
  for (let vid of document.getElementsByTagName("video")) {
    vid.addEventListener('play', (e) => {
      const timeRegex = /#t=(\d+)/;
      const matches = e.target.currentSrc.match(timeRegex);
      if (matches.length > 0) {
        e.target.currentTime = matches[1];
      }
    });
  }
}

// Replace date string on title slide with current date 
// if string provided for date in yaml header is "today"
function currentDate() {
  var date = document.getElementsByClassName("date")[0]
  var dateString = date.textContent;

  var today = new Date().toISOString().substr(0, 10);

  if (dateString == "today") {
    date.textContent = today;
  }
}


function quizzes() {
  matchings();
  multipleChoice();
  freetextAnswerButton();
}

// Adds event listeners for dragging and dropping to the elements of "matching" questions
function matchings() {

  // var retryButtons = document.getElementsByClassName("retryButton");
  // for (let button of retryButtons) {
  //   var initial = button.closest(".matching");
  //   initial.style.display = "none";
  //   button.appendChild(initial);
  // }

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
  shuffleDraggables();
  matchingAnswerButton();
  retryButtons();
}

function retryButtons() {
  var buttons = document.getElementsByClassName("retryButton");

  for (let button of buttons) {
    const initial = button.closest(".matching").cloneNode(true);

    button.onclick = function () {
      var curr = button.closest(".matching");
      curr.parentNode.replaceChild(initial, curr);
      matchings();
    }
  }
}

// Shuffle draggables so the correct pairings aren't always directly below each other
function shuffleDraggables() {
  var dragzones = document.getElementsByClassName("dragzone");
  for (let container of dragzones) {
    var elementsArray = Array.prototype.slice.call(container.getElementsByClassName('draggable'));
    elementsArray.forEach(function (element) {
      container.removeChild(element);
    })
    shuffleArray(elementsArray);
    elementsArray.forEach(function (element) {
      container.appendChild(element);
    })
  }
}

// Fisher-Yates (aka Knuth) Shuffle
function shuffleArray(array) {
  var currentIndex = array.length, temporaryValue, randomIndex;

  // While there remain elements to shuffle...
  while (0 !== currentIndex) {
    // Pick a remaining element...
    randomIndex = Math.floor(Math.random() * currentIndex);
    currentIndex -= 1;

    // And swap it with the current element.
    temporaryValue = array[currentIndex];
    array[currentIndex] = array[randomIndex];
    array[randomIndex] = temporaryValue;
  }

  return array;
}

/*
  Provides the functionality of the "show solution" button for matching questions
*/
function matchingAnswerButton() {
  var answerButtons = document.getElementsByClassName("matchingAnswerButton");

  for (let button of answerButtons) {
    button.onclick = function () {
      var matchingField = button.closest(".matching");
      var dropzones = matchingField.getElementsByClassName("dropzone");

      for (let drop of dropzones) {
        var draggables = drop.getElementsByClassName("draggable");

        // Alert if there's any empty dropzone (i.e. not all pairs are completed)
        if (draggables.length == 0) {
          alert("Please complete all pairs.");
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
      button.nextSibling.disabled = true;
      button.disabled = true;
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
  if (ev.target.className == "draggable") {
    return false;
  }
  ev.target.appendChild(document.getElementById(data));
  ev.target.disabled = true;
}

/*
Handles Multiple choice questions
(Choosing/clicking and coloring of answers. Showing correct solutions etc)
*/
function multipleChoice() {
  const surveys = document.getElementsByClassName("survey");
  let survey_num = 0;
  for (let survey of surveys) {
    survey.setAttribute("data-survey-num", survey_num);
    const local_survey_num = survey_num;
    survey_num += 1;
    var answerButton = survey.getElementsByClassName("mcAnswerButton")[0];
    const answers = survey.getElementsByTagName("li");
    let defBorder = answers[0].style.border;

    let answer_num = 0;
    // highlight chosen answer(s)
    for (let answer of answers) {
      const local_answer_num = answer_num;

      answer.addEventListener("click", function () {
        if (answer.style.border == defBorder) {
          answer.style.border = "thick solid black";
        }
        else {
          answer.style.border = defBorder;
        }
      });
      answer_num += 1;
    }

    // Show correct solutions, lock all interaction with answers
    // Popup if no box was selected
    answerButton.onclick = function () {
      let answered = false;
      for (let answer of answers) {
        if (answer.style.border == defBorder) {
          continue;
        }
        else {
          answered = true;
        }
      }

      if (answered) {
        answerButton.disabled = true;
        for (let answer of answers) {
          var answer_div = answer.getElementsByClassName("answer")[0];
          const is_right = answer_div.classList.contains("right");
          answer.style.backgroundColor = (is_right) ? "#97ff7a" : "#ff7a7a";
          const tooltips = answer.getElementsByClassName("tooltip");
          for (let tooltip of tooltips) {
            tooltip.style.display = "inline";
          }
          answer.style.pointerEvents = "none";
        }
      }
      else {
        alert("No answer chosen!");
        return false;
      }
    };
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
      // Has the user entered anything?
      if (questionField.value) {
        var answer = this.getElementsByClassName('freetextAnswer')[0];
        answer.style.display = 'block';
        answer.style.color = "black";
        if (questionField.value.toLowerCase().trim() == answer.textContent.trim().toLowerCase()) {
          questionField.style.backgroundColor = "rgb(151, 255, 122)";
        }
        else {
          questionField.style.backgroundColor = "rgb(255, 122, 122)";
        }
        questionField.disabled = true;
        button.disabled = true;
      }
      else {
        alert("No answer entered!");
        return false;
      }
    }
  }
}

// Allows printPdf() function to be called as onclick event directly from HTML elements
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