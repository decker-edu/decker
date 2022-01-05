// reference to Reveal deck
let Reveal;

// state of ballot server: "not_init", "open", "closed"
let serverState;

// state of ballot
let ballotState;

// timer for updating #votes
let timerVotes, timerStatus;

// get quiz server
let server;

// the chart object
let myChart;

// get path of script -> used for loading audio files
const path = scriptPath();
function scriptPath() {
  // obtain plugin path from the script element
  // !!! script has to be named "quiz.js" !!!
  const url = new URL(import.meta.url);
  return url.pathname.slice(0, -7);
}

// generate DIV for chart
let chart_div = document.createElement("div");
chart_div.classList.add("overlay");
chart_div.classList.add("visible");
chart_div.setAttribute("data-prevent-swipe", "");
chart_div.style.visibility = "hidden";
chart_div.style.zIndex = "34";
chart_div.style.position = "absolute";
chart_div.style.left = "auto";
chart_div.style.top = "auto";
chart_div.style.right = "10px";
chart_div.style.bottom = "10px";
chart_div.style.width = "420px";
chart_div.style.height = "320px";
chart_div.style.margin = "auto";
chart_div.style.padding = "5px";
chart_div.style.textAlign = "center";
chart_div.style.border = "3px solid #2a9ddf";
chart_div.style.borderRadius = "10px";
chart_div.style.boxShadow = "3px 5px 5px grey";
chart_div.style.backgroundColor = "rgba(255,255,255,1.0)";
chart_div.style.transition = "none";
chart_div.style.transformOrigin = "bottom right";
document.querySelector(".reveal").appendChild(chart_div);

// generate canvas for the actual chart
let chart = document.createElement("canvas");
chart.width = "400";
chart.height = "300";
chart.style.top = "0px";
chart.style.left = "0px";
chart.style.width = "400px";
chart.style.height = "300px";
chart_div.appendChild(chart);

// generate label for #votes
let votes_div = document.createElement("div");
votes_div.classList.add("overlay");
votes_div.classList.add("visible");
votes_div.setAttribute("data-prevent-swipe", "");
votes_div.style.visibility = "hidden";
votes_div.style.zIndex = "33";
votes_div.style.position = "absolute";
votes_div.style.left = "auto";
votes_div.style.top = "auto";
votes_div.style.right = "10px";
votes_div.style.bottom = "10px";
votes_div.style.width = "auto";
votes_div.style.height = "auto";
votes_div.style.margin = "auto";
votes_div.style.padding = "5px";
votes_div.style.color = "black";
votes_div.style.fontSize = "20px";
votes_div.style.textAlign = "center";
votes_div.style.border = "3px solid #2a9ddf";
votes_div.style.borderRadius = "10px";
votes_div.style.boxShadow = "3px 5px 5px grey";
votes_div.style.backgroundColor = "rgba(255,255,255,1.0)";
votes_div.style.cursor = "help";
votes_div.style.transformOrigin = "bottom right";
document.querySelector(".reveal").appendChild(votes_div);

// generate QR code for DIVs with class/id "quiz-qr"
let head = document.querySelector("head");
let qrscript = document.createElement("script");
qrscript.type = "text/javascript";
qrscript.src = path + "qrcode.min.js";
qrscript.onload = function () {
  let e = document.getElementById("quiz-qr");
  if (e) e.classList.add("quiz-qr");
  Array.from(document.getElementsByClassName("quiz-qr")).forEach(function (e) {
    let size = parseInt(e.style.width, 10) || 300;
    let qrcode = new QRCode(e, {
      text: server,
      width: size,
      height: size,
      colorDark: "#000000",
      colorLight: "#ffffff",
      correctLevel: QRCode.CorrectLevel.H,
    });
    qrcode.makeCode(server);
  });
};
head.appendChild(qrscript);

// write server URL in elements with class/id 'quiz-url'
let e = document.getElementById("quiz-url");
if (e) e.classList.add("quiz-url");
Array.from(document.getElementsByClassName("quiz-url")).forEach(function (e) {
  e.innerHTML = server;
});

// load WWM jingles
let jingleQuestion = new Audio(path + "/wwm-question.mp3");
let jingleAnswer = new Audio(path + "/wwm-answer.mp3");

// how many answers on current slide?
let numAnswers = 0;

// what to do on slide change
function slideChanged() {
  // if not presenter displayw slide preview
  if (!Reveal.isSpeakerNotes()) {
    // stop sounds
    jingleQuestion.pause();
    jingleAnswer.pause();

    // if quiz is active -> close it
    // (but only then, since it requires authentication)
    if (serverState == "open") {
      closeBallot();
    }

    // hide stuff
    hideVotes();
    hideChart();

    // remove timers
    clearInterval(timerVotes);
    clearInterval(timerStatus);

    // reset state
    ballotState = "not_init";
    numAnswers = 0;

    // is this a quiz slide? -> find answers (new version)
    Reveal.getCurrentSlide()
      .querySelectorAll('.reveal .quiz ul>li>input[type="checkbox"]')
      .forEach((input) => {
        input.parentElement.classList.remove("show-answer");
      });
  }
}

// ballot stuff (needs authentication) -----------------------------------

// start new ballot
function startBallot() {
  let xhr = new XMLHttpRequest();
  xhr.open("post", server + "/init/" + numAnswers, false);
  xhr.withCredentials = true;
  xhr.send(null);
  jingleQuestion.currentTime = 0;
  jingleQuestion.play();
}

// close ballot
function closeBallot() {
  let xhr = new XMLHttpRequest();
  xhr.open("post", server + "/close", true);
  xhr.withCredentials = true;
  xhr.send(null);
}

// #votes stuff ----------------------------------------------------------

// show votes div
function showVotes() {
  votes_div.style.visibility = "visible";
  timerVotes = setInterval(getVotes, 1000);
  timerStatus = setInterval(getStatus, 1000);
}

// hide votes div
function hideVotes() {
  votes_div.style.visibility = "hidden";
  clearInterval(timerVotes);
  clearInterval(timerStatus);
}

// get number of votes from server
function getVotes() {
  let xhr = new XMLHttpRequest();
  xhr.open("get", server + "/count", true);
  xhr.setRequestHeader("Content-Type", "text/plain");
  xhr.addEventListener("load", function () {
    drawVotes(JSON.parse(xhr.response));
  });
  xhr.send();
}

// draw number of votes
function drawVotes(nVotes) {
  votes_div.innerHTML = nVotes;
}

// get status from server
function getStatus() {
  let xhr = new XMLHttpRequest();
  xhr.open("get", server + "/status", true);
  xhr.setRequestHeader("Content-Type", "text/plain");
  xhr.addEventListener("load", function () {
    drawStatus(JSON.parse(xhr.response));
  });
  xhr.send();
}

// draw number of votes
function drawStatus(s) {
  serverState = s;
  if (s == "open") {
    votes_div.style.border = "3px solid green";
  } else {
    votes_div.style.border = "3px solid red";
  }
}

// chart stuff -----------------------------------------------------------

// hide chart
function hideChart() {
  chart_div.style.visibility = "hidden";
}

// show/hide chart of answers
function showChart() {
  // hide #votes
  votes_div.style.visibility = "hidden";

  // setup new result request and add trigger drawChart
  let xhr = new XMLHttpRequest();
  xhr.open("get", server + "/result", true);
  xhr.setRequestHeader("Content-Type", "text/plain");
  xhr.addEventListener("load", function () {
    drawChart(JSON.parse(xhr.response));
  });
  xhr.send();
}

// draw chart; result is JSON string of results
function drawChart(result) {
  // play sound
  jingleAnswer.currentTime = 0;
  jingleAnswer.play();

  // resize and show
  chart_div.style.visibility = "visible";

  // DEBUG
  //result = [10, 26, 5, 7];

  // how many answers?
  let n = 0;
  for (let i = 0; i < result.length; i++) {
    n += result[i];
  }

  // convert answers to percentages
  for (let i = 0; i < result.length; i++) {
    result[i] /= n;
  }

  // destroy chart if it was created before (strictly required!)
  if (myChart) {
    myChart.destroy();
  }

  // get labels as subset of this array
  let labels = ["A", "B", "C", "D", "E", "F", "G", "H"].slice(0, result.length);

  // (re)create chart
  let ctx = chart.getContext("2d");
  myChart = new Chart(ctx, {
    type: "bar",
    data: {
      labels: labels,
      datasets: [
        {
          data: result,
          backgroundColor: "#2a9ddf",
        },
      ],
    },
    options: {
      animation: { duration: 3000 },
      legend: { display: false },
      title: { display: false },
      tooltips: { enabled: false },
      scales: {
        yAxes: [
          {
            gridLines: { display: false },
            ticks: { beginAtZero: true },
          },
        ],
      },
    },
  });
}

// ballot states
function switchBallotState() {
  //console.log("old ballot state: " + ballotState);

  switch (ballotState) {
    case "not_init":
      if (numAnswers) {
        startBallot();
        showVotes();
        ballotState = "open";
      } else {
        // just to trigger authentication
        closeBallot();
      }
      break;

    case "open":
      if (serverState != "open") {
        startBallot();
      } else {
        closeBallot();
        hideVotes();
        showChart();
        ballotState = "chart";
      }
      break;

    case "chart":
      hideChart();
      ballotState = "done";
      break;

    case "done":
      showChart();
      ballotState = "chart";
      break;

    default:
      console.error("this should not happen");
      hideChart();
      hideVotes();
      ballotState = "not_init";
      break;
  }
}

function resize() {
  let s = Reveal.getScale();
  chart_div.style.transform = "scale(" + s + ")";
  votes_div.style.transform = "scale(" + s + ")";
}

const Plugin = {
  id: "quizPlugin",
  init: (deck) => {
    Reveal = deck;

    // get config
    const config = Reveal.getConfig().quiz || {};
    server = config.server || "http://graphics.uni-bielefeld.de:8080";

    // setup keyboard shortcut
    Reveal.removeKeyBinding(81);
    Reveal.addKeyBinding(
      { keyCode: 81, key: "Q", description: "Toggle Quiz" },
      switchBallotState
    );

    // add event listener
    Reveal.addEventListener("slidechanged", slideChanged);
    Reveal.addEventListener("resize", resize);

    // prepare quizzes
    document
      .querySelectorAll('.reveal .quiz ul>li>input[type="checkbox"]')
      .forEach((input) => {
        let li = input.parentElement;

        li.setAttribute("role", "button");
        li.setAttribute("tabindex", 0);
        li.classList.add(input.checked ? "right" : "wrong");

        li.onclick = function (e) {
          this.classList.add("show-answer");
        };

        li.onkeydown = function (e) {
          if (e.code == "Space" || e.code == "Enter") {
            this.classList.add("show-answer");
            e.preventDefault();
            e.stopPropagation();
          }
        };
      });
  },
};

export default Plugin;
