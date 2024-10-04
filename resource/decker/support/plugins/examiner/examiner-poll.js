import { pollSession } from "./poll.js";

export async function preparePolls(reveal) {
  
  // Only when reveal is ready we can find the elements we need.
  let color = Decker.meta["css-light-colors"];

  let session = null;
  let polls = [];

  try {
    session = await pollSession({
      serverUrl: Decker.meta["poll-server"],
      clientCss: clientCssTemplate(color),
    });
  } catch (err) {
    Decker.flash.message("Connection to poll server failed.");
    return {
      close: () => { },
    };
  }

  let { id, url } = session.sessionId();
  Decker.flash.message(
    `Connected to poll session <strong style="color:var(--accent6)">${id}</strong>`
  );

  let revealElement = reveal.getRevealElement();
  let qrcodeContainer = document.createElement("div");

  qrcodeContainer.classList.add("qrcode", "container");
  qrcodeContainer.innerHTML = `
    <canvas id="poll-qrcode-canvas"></canvas>
    <span><a 
      href="${url}" 
      target="_blank" 
      title="${url}" 
      id="poll-session-id">${id}
    </a></span>
  `;
  qrcodeContainer.addEventListener("click", () => {
    qrcodeContainer.classList.remove("show");
  });

  revealElement.setAttribute("data-poll-session", id);
  revealElement.appendChild(qrcodeContainer);

  session.fillQRCode("poll-qrcode-canvas");

  let callbackLog = [];

  // Prep all questions in the document marked with class 'poll'.
  let questions = document.querySelectorAll("div.question.poll");
  for (let question of questions) {
    let title = question.getAttribute("data-title");
    let lectureId = question.getAttribute("data-lecture-id");
    let topicId = question.getAttribute("data-topic-id");
    console.log(title, lectureId, topicId);
    // Only consider multiple choice for now
    let answer = question.querySelector("div.answer.exa-mc");
    let nvotes = parseInt(answer.getAttribute("data-votes")) || 1;
    if (!answer) return;

    // Wire the button for the QR Code.
    let qrcodeButton = question.querySelector("button.qrcode");
    let qrcode = () => {
      qrcodeContainer.classList.add("show");
    };
    qrcodeButton.addEventListener("click", qrcode);
    callbackLog.push({
      button: qrcodeButton,
      event: "click",
      callback: qrcode,
    });

    // Collect the vote feedback structures.
    let voteMap = {};
    let solution = [];
    let results = [];
    let voteBlocks = answer.querySelectorAll("div.choice div.vote");
    let choiceElements = answer.querySelectorAll("div.choice");
    for (let choice of choiceElements) {
      let vb = choice.querySelector("div.vote");
      let label = vb.getAttribute("label");
      let votesElement = vb.querySelector("div.vote div.votes");
      voteMap[label] = votesElement;
      if (choice.classList.contains("correct")) solution.push(label);
    }
    let choices = Object.keys(voteMap);

    // The buttons
    let pollButton = question.querySelector("button.poll");
    let stopButton = question.querySelector("button.stop");
    stopButton.setAttribute("disabled", true);
    pollButton.removeAttribute("disabled");

    let abort = () => {
      session.stop();
      session.reset();
    };

    // Wire the Poll button that starts the poll.
    const poll = (e) => {
      let restoreStop = stopButton.textContent;
      session.poll(choices, solution, nvotes, {
        onActive: (participants, votes, complete) => {
          displayVotes(votes, voteMap);
          showVotes(voteBlocks, true);
          const voted = Object.values(votes).reduce((t, v) => t + v, 0);
          stopButton.textContent = ` ${complete}/${participants} `;
          stopButton.removeAttribute("disabled");
          pollButton.setAttribute("disabled", false);
          reveal.on("slidechanged", abort);
        },
        onFinished: (participants, votes, complete) => {
          displayVotes(votes, voteMap);
          showVotes(voteBlocks, false);
          saveVotes(title, lectureId, topicId, votes, polls);
          uploadPolls(id, polls);
          const voted = Object.values(votes).reduce((t, v) => t + v, 0);
          stopButton.textContent = restoreStop;
          stopButton.setAttribute("disabled", false);
          pollButton.removeAttribute("disabled");
          reveal.off("slidechanged", abort);
        },
      }, "Random");
    };
    pollButton.addEventListener("click", poll);
    callbackLog.push({ button: pollButton, event: "click", callback: poll });

    // Wire the Stop button.
    const stop = (e) => {
      session.stop();
    };
    stopButton.addEventListener("click", stop);
    callbackLog.push({ button: stopButton, event: "click", callback: stop });
  }

  return {
    close: () => {
      session.stop();
      session.reset();
      revealElement.removeChild(qrcodeContainer);
      revealElement.removeAttribute("data-poll-session");

      for (let cb of callbackLog) {
        cb.button.removeEventListener(cb.event, cb.callback);
      }
    },
  };
}

// const totalVotes = Object.values(votes).reduce((t, v) => t + v, 0);

function showVotes(voteMap, show) {
  for (let vote of Object.values(voteMap))
    if (show) vote.classList.add("polling");
    else vote.classList.remove("polling");
}


function saveVotes(title, lectureId, topicId, votes, polls) {
  if (Decker.meta["save-polls"])
    polls.push({ title, lectureId, topicId, votes });
}

function uploadPolls(id, polls) {
  if (Decker.meta["save-polls"]) {
    let path = location.pathname;
    let base = path.substring(0, path.lastIndexOf("-"));
    let url = base + `-${id}-poll.json`
    fetch(url, { method: "PUT", body: JSON.stringify(polls) })
      .then((r) => console.log("[] poll data uploaded to:", url))
      .catch((e) => {
        console.error("[] cannot upload poll data to:", url, "reason:", e);
      });
  }
}

function displayVotes(votes, voteMap) {
  const min = 0;
  const max = 100;
  const maxVotes = Math.max(
    1,
    Object.values(votes).reduce((t, v) => Math.max(t, v), 0)
  );
  for (let [label, count] of Object.entries(votes)) {
    let percent = min + (count / maxVotes) * (max - min);
    voteMap[label].setAttribute("style", `width:${percent}%;`);
    voteMap[label].textContent = votes[label];
  }
}

function clientCssTemplate(color) {
  return `
    html {
      color: ${color.shade7};
      background-color: ${color.shade0};
    }
    h1#pollid {display: none}
    p#nvotes {display: none}
    body.polling p#status {display: none}
    body.connected.winner {
      background-color: ${color.accent1};
    }
    body.connected.winner #buttons { 
      display: none 
    }
    body.connected.winner p#status::before {
      content: "Gewonnen!";
    }
    body.connected.winner #status {
      font-size: 14vmin;
      color: gold;
      font-weight: bold;
      -webkit-text-stroke: 0.03em black;
      text-shadow:
        0.03em 0.03em 0 black,
        -0.01em -0.01em 0 black,  
        0.01em -1px 0 black,
        -0.01em 1px 0 black,
        0.01em 1px 0 black;
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
    button {
      color: ${color.shade7} !important;
      border: 0 !important;
    }
    button#A {
      background-color: ${color["accent0-bbg"]};
    }
    button#B {
      background-color: ${color["accent1-bbg"]};
    }
    button#C {
      background-color: ${color["accent2-bbg"]};
    }
    button#D {
      background-color: ${color["accent3-bbg"]};
    }
    button#E {
      background-color: ${color["accent4-bbg"]};
    }
    button#F {
      background-color: ${color["accent5-bbg"]};
    }
    button#G {
      background-color: ${color["accent6-bbg"]};
    }
    button#H {
      background-color: ${color["accent7-bbg"]};
    }
    button.checked {
      color: ${color["shade1"]} !important;
    }
    button#A.checked {
      background-color: ${color["accent0"]};
    }
    button#B.checked {
      background-color: ${color["accent1"]};
    }
    button#C.checked {
      background-color: ${color["accent2"]};
    }
    button#D.checked {
      background-color: ${color["accent3"]};
    }
    button#E.checked {
      background-color: ${color["accent4"]};
    }
    button#F.checked {
      background-color: ${color["accent5"]};
    }
    button#G.checked {
      background-color: ${color["accent6"]};
    }
    button#H.checked {
      background-color: ${color["accent7"]};
    }
    button[disabled]:not(.checked) {
      color: ${color["shade4"]} !important;
      background-color: ${color["shade1"]} !important;
    }
    button#A[disabled].checked {
      background-color: ${color["accent0-bg"]};
    }
    button#B[disabled].checked {
      background-color: ${color["accent1-bg"]};
    }
    button#C[disabled].checked {
      background-color: ${color["accent2-bg"]};
    }
    button#D[disabled].checked {
      background-color: ${color["accent3-bg"]};
    }
    button#E[disabled].checked {
      background-color: ${color["accent4-bg"]};
    }
    button#F[disabled].checked {
      background-color: ${color["accent5-bg"]};
    }
    button#G[disabled].checked {
      background-color: ${color["accent6-bg"]};
    }
    button#H[disabled].checked {
      background-color: ${color["accent7-bg"]};
    }
    @media (hover) {
      button#A:not([disabled]):hover {
        background-color: ${color["accent0-bg"]};
      }
      button#B:not([disabled]):hover {
        background-color: ${color["accent1-bg"]};
      }
      button#C:not([disabled]):hover {
        background-color: ${color["accent2-bg"]};
      }
      button#D:not([disabled]):hover {
        background-color: ${color["accent3-bg"]};
      }
      button#E:not([disabled]):hover {
        background-color: ${color["accent4-bg"]};
      }
      button#F:not([disabled]):hover {
        background-color: ${color["accent5-bg"]};
      }
      button#G:not([disabled]):hover {
        background-color: ${color["accent6-bg"]};
      }
      button#H:not([disabled]):hover {
        background-color: ${color["accent7-bg"]};
      }
      button#A:not([disabled]).checked:hover {
        background-color: ${color["accent0-fg"]};
      }
      button#B:not([disabled]).checked:hover {
        background-color: ${color["accent1-fg"]};
      }
      button#C:not([disabled]).checked:hover {
        background-color: ${color["accent2-fg"]};
      }
      button#D:not([disabled]).checked:hover {
        background-color: ${color["accent3-fg"]};
      }
      button#E:not([disabled]).checked:hover {
        background-color: ${color["accent4-fg"]};
      }
      button#F:not([disabled]).checked:hover {
        background-color: ${color["accent5-fg"]};
      }
      button#G:not([disabled]).checked:hover {
        background-color: ${color["accent6-fg"]};
      }
      button#H:not([disabled]).checked:hover {
        background-color: ${color["accent7-fg"]};
      }
    }
    `;
}
