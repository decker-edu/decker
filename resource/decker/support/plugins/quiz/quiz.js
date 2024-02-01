// Henrik's server API
import { pollSession } from "../examiner/poll.js";

window.displayTooltip = function (element, text) {
  const tooltip = document.createElement("div");
  tooltip.classList.add("fading-tooltip");
  const wedge = document.createElement("div");
  wedge.classList.add("wedgedown");
  tooltip.appendChild(wedge);
  const message = document.createElement("span");
  message.classList.add("tooltip-text");
  message.innerText = text;
  tooltip.appendChild(message);
  document.body.appendChild(tooltip);
  const box = element.getBoundingClientRect();
  const tbox = tooltip.getBoundingClientRect();
  tooltip.style.top = `${Math.floor(box.top) - 64}px`;
  tooltip.style.left = `${Math.floor(
    box.left - (tbox.width - box.width) / 2
  )}px`;
  setTimeout(() => {
    tooltip.classList.add("fade");
    tooltip.addEventListener("transitionend", () => {
      tooltip.remove();
    });
  }, 1000);
};

// reference to Reveal deck
let Reveal;

// poll info on current slide
let numAnswers = 0;
let numCorrectAnswers;
let solution;

// polling
let session;
let qrcode, qrcodeCanvas, qrcodeLink;
let connectionIndicator;
let finalVotes;
let pollState;
let myChart;

// GUI elements
let votes_div, chart_div, chart;

// config
const serverUrl =
  Decker.meta.polling?.server ||
  Decker.meta["poll-server"] ||
  "wss://decker.cs.tu-dortmund.de/quizzer/quiz";
const winnerSelection = Decker.meta.polling?.selection || "Random";
console.log("Polling URL: ", serverUrl);
console.log("Polling Selection: ", winnerSelection);

// get path of script -> used for loading audio files
const url = new URL(import.meta.url);
const path = url.pathname.substring(0, url.pathname.lastIndexOf("/"));
const href = url.href.substring(0, url.href.lastIndexOf("/"));

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

function setConnectionIndicator(what) {
  switch (what) {
    case "ok":
      {
        connectionIndicator.classList.remove("error");
        connectionIndicator.classList.remove("warning");
        connectionIndicator.classList.add("ok");
        connectionIndicator.title = "Connected to the Quiz Server";
      }
      break;
    case "error":
      {
        connectionIndicator.classList.remove("ok");
        connectionIndicator.classList.remove("warning");
        connectionIndicator.classList.add("error");
        connectionIndicator.title =
          "Disconnected from the Quiz Server\nYou may attempt to reconnect by trying to interact with the poll";
      }
      break;
    case "warning":
      {
        connectionIndicator.classList.remove("ok");
        connectionIndicator.classList.remove("error");
        connectionIndicator.classList.add("warning");
        connectionIndicator.title = "Missing responses from Quiz Server";
      }
      break;
    case "unknown":
      {
        connectionIndicator.classList.remove("ok");
        connectionIndicator.classList.remove("error");
        connectionIndicator.title = "Not yet connected to Quiz Server";
      }
      break;
  }
}

function setupGUI() {
  if (!Reveal.hasPlugin("ui-anchors")) console.error("need ui-anchors");

  const revealElement = Reveal.getRevealElement();
  const anchors = Reveal.getPlugin("ui-anchors");

  connectionIndicator = createElement({
    type: "button",
    id: "quiz-connection-indicator",
    classes: "fa-button fas fa-wifi poll-only presenter-only",
    tooltip: "Not yet connected to Quiz Server",
  });
  anchors.addBottomCenterButton(connectionIndicator);

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
    onclick: issueStateSwitch,
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

  qrcode = createElement({
    type: "div",
    id: "qrcode-container",
    parent: document.body,
  });
  qrcode.addEventListener("click", () => {
    qrcode.classList.remove("show");
  });

  qrcodeCanvas = createElement({
    type: "canvas",
    id: "qrcode-canvas",
    parent: qrcode,
  });
  qrcodeCanvas.addEventListener("click", (evt) => {
    qrcodeCanvas.classList.toggle("smaller");
    evt.stopPropagation();
  });

  qrcodeLink = createElement({
    type: "a",
    id: "qrcode-link",
    parent: qrcode,
  });

  const closeButton = createElement({
    type: "button",
    id: "close-qr-button",
    classes: "fa-button fas fa-close",
    tooltip: "Close QR code",
    parent: qrcode,
    onclick: () => {
      qrcode.classList.toggle("show");
    },
  });
}

// what to do on slide change
function slideChanged() {
  // if not presenter display slide preview
  if (!Reveal.isSpeakerNotes()) {
    // stop sounds
    jingleQuestion.pause();
    jingleAnswer.pause();

    // hide stuff
    if (pollState === "open") abortPoll();
    hideVotes();
    hideChart();

    // reset state
    pollState = "not_init";
    numAnswers = 0;
    numCorrectAnswers = 0;
    solution = [];

    // is this a quiz slide? -> find answers
    const slide = Reveal.getCurrentSlide();
    const inputElements = slide.querySelectorAll(
      '.reveal .quiz ul>li>input[type="checkbox"]'
    );
    numAnswers = inputElements.length;
    const choices = ["A", "B", "C", "D", "E", "F", "G", "H"];
    for (let i = 0; i < numAnswers; i++) {
      const input = inputElements[i];
      input.parentElement.classList.remove("show-answer");
      if (input.checked) {
        ++numCorrectAnswers;
        solution.push(choices[i]);
      }
    }

    // set poll class in reveal element
    Reveal.getViewportElement().classList.toggle("poll", numAnswers > 0);
  }
}

function playJingleQuestion() {
  jingleQuestion.currentTime = 0;
  jingleQuestion.play();
}

function playJingleAnswer() {
  jingleAnswer.currentTime = 0;
  jingleAnswer.play();
}

async function startPoll() {
  // initialize on first start
  if (!session) await startPollingSession();

  // get labels as subset of this array
  let choices = ["A", "B", "C", "D", "E", "F", "G", "H"].slice(0, numAnswers);

  // this does not fail if the connection is broken
  session.poll(
    choices,
    solution,
    numCorrectAnswers ? numCorrectAnswers : 1,
    // If we leave it at 0 if there are no correct answers the poll states goes negative ...
    // If we set this to numAnswers the poll isn't "done" unless the user checks ALL answers
    {
      onReady: () => {
        setConnectionIndicator("ok");
      },
      onActive: (participants, votes, complete) => {
        if (pollState === "not_init") {
          pollState = "open";
          showVotes();
          playJingleQuestion();
        }
        votes_div.textContent = `${complete} / ${participants}`;
        setConnectionIndicator("ok");
      },
      onFinished: (participants, votes, complete) => {
        if (pollState === "open") {
          pollState = "chart";
          hideVotes();
          playJingleAnswer();
        }
        finalVotes = votes;
        createChart();
        showChart();
        setConnectionIndicator("ok");
      },
      onError: () => {
        window.displayTooltip(connectionIndicator, "Connection Lost");
      },
    },
    winnerSelection
  );
  startTimeout = setTimeout(() => {
    setConnectionIndicator("error");
  });
}

function stopPoll() {
  if (session) session.stop();
}

function abortPoll() {
  if (session) session.reset();
}

function showVotes() {
  votes_div.style.display = "inline";
}

function hideVotes() {
  votes_div.style.display = "none";
}

async function toggleQR() {
  if (!session) await startPollingSession();
  if (!session.getData().socket) {
    session.reset(); // Try to trigger a reconnect
  }
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

  // destroy chart if it was created before (strictly required!)
  if (myChart) {
    myChart.destroy();
  }

  // let labels = ["A", "B", "C", "D"];
  // let result = [1, 2, 0, 8];
  let labels = [];
  let result = [];
  for (let [label, count] of votes) {
    labels.push(label);
    result.push(count);
  }
  const sum = Math.max(
    1,
    result.reduce((a, b) => a + b, 0)
  );
  const data = result.map((c) => c / sum);
  // console.log(labels, result, data);

  // (re)create chart
  let ctx = chart.getContext("2d");
  myChart = new Chart(ctx, {
    type: "bar",
    data: {
      labels: labels,
      datasets: [
        {
          data: data,
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
      },
      scales: {
        y: {
          min: 0,
          max: 1,
          ticks: { format: { style: "percent" } },
        },
      },
    },
  });
}

function issueStateSwitch() {
  switch (pollState) {
    case "not_init":
      if (numAnswers) {
        startPoll();
      }
      break;
    case "open":
      stopPoll();
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
      console.error("pollState has an invalid value: this should not happen");
      hideChart();
      hideVotes();
      pollState = "not_init";
      break;
  }
}

function prepareQuizzes() {
  document
    .querySelectorAll('.reveal .quiz ul>li>input[type="checkbox"]')
    .forEach((input) => {
      let li = input.parentElement;

      // active quizzes
      if (!Decker.meta["disable-quizzes"]) {
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
      }
      // do not activate; instead hide correct/incorrect
      else {
        input.removeAttribute("checked");
        li.classList.remove("task-yes", "task-no");
      }
    });
}

async function startPollingSession() {
  console.log("starting new polling session");

  // connect to server
  session = await pollSession({
    serverUrl: serverUrl,
    clientCss: `
    html {
      color: #333
      background-color: #ccc;
    }
    h1#pollid {
      display: none;
    }
    p#nvotes {
      display: none;
    }
    body {
      justify-content: center;
      align-items: center;
    }
    body.polling p#status {
      display: none;
    }
    button.checked {
      color: white;
      background-color: #2a9ddf !important;
    }
    #buttons { 
      width: 100%;
    }
    body.winner #buttons { 
      display: none 
    }
    body.winner p#status::before { 
      content: "${
        document.documentElement.lang === "de" ? "GEWONNEN!" : "YOU WON!"
      }"; 
    }
    body.winner #status {
      font-size: 14vmin;
      color: gold;
      font-weight: bold;
      -webkit-text-stroke: 0.03em black;
      text-shadow:
        0.03em 0.03em 0 #000,
        -0.01em -0.01em 0 #000,  
        0.01em -1px 0 #000,
        -0.01em 1px 0 #000,
        0.01em 1px 0 #000;
      animation: wiggle 1s infinite;
    }
    @keyframes wiggle {
      0%,40%,100% {
        transform: rotate(-10deg);
      }
      20% {
        transform: rotate(10deg);
      }
    }
    `,
    onready: () => {
      setConnectionIndicator("ok");
    },
    onclose: () => {
      console.log("polling session was closed");
      setConnectionIndicator("error");
      displayTooltip(connectionIndicator, "Lost Connection");
      Reveal.off("slidechanged", abortPoll);
    },
    onwarning: () => {
      console.log("connection is getting spotty ...");
      setConnectionIndicator("warning");
    },
  });
  setConnectionIndicator("ok");

  // create QR code
  let { id, secret, url, socket } = session.getData();
  qrcodeLink.innerHTML = String.raw`${url}`;
  qrcodeLink.href = url;
  qrcodeLink.target = "_blank";
  session.fillQRCode("qrcode-canvas");
}

const Plugin = {
  id: "quizPlugin",
  init: (deck) => {
    Reveal = deck;
    Reveal.addEventListener("slidechanged", slideChanged);
    setupGUI();
    if (!Decker.meta["disable-quizzes"]) {
      prepareQuizzes();
    }
  },
};

export default Plugin;
