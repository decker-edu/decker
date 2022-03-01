// Henrik's server API
import { pollSession } from "../examiner/poll.js";

// we need Chart.js
import { Chart, registerables } from "../charts/chart.esm.js";
Chart.register(...registerables);
import "../charts/plugin-csszoom.js";
import "../charts/plugin-colorschemes.js";

// reference to Reveal deck
let Reveal;

// how many answers on current slide?
let numAnswers = 0;
let singleChoice = true;

// polling
let session;
let qrcode;
let finalVotes;
let pollState;
let myChart;

// GUI elements
let votes_div, chart_div, chart;

// get path of script -> used for loading audio files
const path = scriptPath();
function scriptPath() {
  const url = new URL(import.meta.url);
  const path = url.pathname;
  return path.substring(0, path.lastIndexOf("/"));
}

// load WWM jingles
let jingleQuestion = new Audio(path + "/wwm-question.mp3");
let jingleAnswer = new Audio(path + "/wwm-answer.mp3");

// GUI helper (uses named parameters)
function createElement({ type, id, classes, tooltip, parent, onclick = null }) {
  let e = document.createElement(type);
  if (id) e.id = id;
  if (classes) e.className = classes;
  if (tooltip) e.title = tooltip;
  if (parent) parent.appendChild(e);
  if (onclick) e.addEventListener("click", onclick);
  return e;
}

function setupGUI() {
  if (!Reveal.hasPlugin("ui-anchors")) console.error("need ui-anchors");

  const revealElement = Reveal.getRevealElement();
  const anchors = Reveal.getPlugin("ui-anchors");

  const qrButton = createElement({
    type: "button",
    classes: "fa-button fas fa-qrcode poll-only presenter-only",
    tooltip: "Show QR code",
    onclick: toggleQR,
  });
  anchors.addBottomCenterButton(qrButton);

  const pollButton = createElement({
    type: "button",
    classes: "fa-button fas fa-poll poll-only presenter-only",
    tooltip: "Start/stop poll",
    onclick: switchPollState,
  });
  anchors.addBottomCenterButton(pollButton);

  votes_div = createElement({
    type: "div",
    id: "poll-votes",
    classes: "poll-only presenter-only",
  });
  anchors.addBottomCenterButton(votes_div);

  chart_div = createElement({
    type: "div",
    id: "poll-chart",
    classes: "overlay visible",
    parent: revealElement,
  });
  chart_div.setAttribute("data-prevent-swipe", "");

  chart = createElement({
    type: "canvas",
    parent: chart_div,
  });
  chart.width = "400";
  chart.height = "300";
}

// what to do on slide change
function slideChanged() {
  // if not presenter display slide preview
  if (!Reveal.isSpeakerNotes()) {
    // stop sounds
    jingleQuestion.pause();
    jingleAnswer.pause();

    // hide stuff
    hideVotes();
    hideChart();

    // reset state
    pollState = "not_init";
    numAnswers = 0;
    let numCorrectAnswers = 0;

    // is this a quiz slide? -> find answers (new version)
    const slide = Reveal.getCurrentSlide();
    slide
      .querySelectorAll('.reveal .quiz ul>li>input[type="checkbox"]')
      .forEach((input) => {
        input.parentElement.classList.remove("show-answer");
        numAnswers++;
        if (input.checked) ++numCorrectAnswers;
      });
    singleChoice = numCorrectAnswers == 1;

    // set poll class in reveal element
    Reveal.getRevealElement().classList.toggle("poll", numAnswers > 0);
  }
}

async function startPoll() {
  // initialize on first start
  if (!session) await preparePolling();

  // get labels as subset of this array
  let choices = ["A", "B", "C", "D", "E", "F", "G", "H"].slice(0, numAnswers);
  let nvotes = singleChoice ? 1 : numAnswers;

  session.poll(choices, nvotes, {
    onActive: (participants, votes, complete) => {
      console.log("Poll:", "active", participants, votes, complete);
      votes_div.textContent = `${complete} / ${participants}`;
      Reveal.on("slidechanged", abortPoll);
    },
    onFinished: (participants, votes, complete) => {
      console.log("Poll:", "finished", participants, votes, complete);
      finalVotes = votes;
      createChart();
      showChart();
      Reveal.off("slidechanged", abortPoll);
    },
  });
}

function stopPoll() {
  session.stop();
}

function abortPoll() {
  session.stop();
  session.reset();
}

function showVotes() {
  votes_div.style.display = "inline";
}

function hideVotes() {
  votes_div.style.display = "none";
}

async function toggleQR() {
  if (!session) await preparePolling();
  qrcode.classList.toggle("show");
}

function showChart() {
  chart_div.style.visibility = "visible";
}

function hideChart() {
  chart_div.style.visibility = "hidden";
}

function createChart() {
  let votes = Object.entries(finalVotes);
  votes.sort((a, b) => a[0].localeCompare(b[0]));

  // play sound
  jingleAnswer.currentTime = 0;
  jingleAnswer.play();

  // destroy chart if it was created before (strictly required!)
  if (myChart) {
    myChart.destroy();
  }

  let labels = [];
  let result = [];
  for (let [label, count] of votes) {
    labels.push(label);
    result.push(count);
  }

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
      animation: {
        duration: 3000,
      },
      plugins: {
        title: { display: false },
        legend: { display: false },
        tooltip: { enabled: false },
      },
      scales: {
        y: {
          min: 0,
          max: 1,
          stepSize: 0.1,
        },
      },
    },
  });
}

// ballot states
function switchPollState() {
  // console.log("old poll state: " + pollState);
  switch (pollState) {
    case "not_init":
      if (numAnswers) {
        startPoll();
        showVotes();
        pollState = "open";
      }
      break;

    case "open":
      hideVotes();
      stopPoll();
      pollState = "chart";
      break;

    case "chart":
      hideChart();
      pollState = "done";
      break;

    case "done":
      showChart();
      pollState = "chart";
      break;

    default:
      console.error("this should not happen");
      hideChart();
      hideVotes();
      pollState = "not_init";
      break;
  }
  // console.log("new poll state: " + pollState);
}

function prepareQuizzes() {
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
}

async function preparePolling() {
  // connect to server
  session = await pollSession({
    serverUrl: Decker.meta["poll-server"],
    clientCss: `
    html {
      color: #333
      background-color: #ccc;
    }
    h1#pollid {display: none}
    p#nvotes {display: none}
    body.polling p#status {display: none}
    button {
    }
    button.checked {
      color: white;
      background-color: #2a9ddf !important;
    }
    `,
  });
  let { id, url } = session.sessionId();

  // create QR code
  qrcode = document.createElement("div");
  qrcode.classList.add("qrcode", "container");
  qrcode.innerHTML = String.raw`
    <canvas id="poll-qrcode-canvas"></canvas>
    <span><a href="${url}" target="_blank" title="${url}" id="poll-session-id">${id}</a> </span>
  `;
  qrcode.addEventListener("click", () => {
    qrcode.classList.remove("show");
  });
  document.body.appendChild(qrcode);
  session.fillQRCode("poll-qrcode-canvas");

  // play jingle
  jingleQuestion.currentTime = 0;
  jingleQuestion.play();
}

const Plugin = {
  id: "quizPlugin",
  init: (deck) => {
    Reveal = deck;
    Reveal.addEventListener("slidechanged", slideChanged);
    setupGUI();
    prepareQuizzes();
  },
};

export default Plugin;
