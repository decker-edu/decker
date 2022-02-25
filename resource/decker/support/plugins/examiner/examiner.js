import { pollSession } from "./poll.js";

const Plugin = {
  id: "Examiner",
  init: (reveal) => {
    reveal.on("ready", (e) => {
      console.log("Poll: Reveal ready");
      prepareExaminer();
      preparePolls(reveal);
    });
  },
};

export default Plugin;

function gradeMC(exam) {
  let mcAnswer = exam.querySelector("div.exa-mc");
  if (mcAnswer !== null) {
    let choices = mcAnswer.querySelectorAll("div.exa-mc div.choice");
    let nCorrectChoices = 0;
    let nCorrect = 0;
    let nWrong = 0;
    for (let choice of choices) {
      let choiceIsCorrect = choice.classList.contains("correct");
      let checkBox = choice.querySelector("div.check-box");
      let isChecked = hasAttribute(checkBox, "checked");

      if (choiceIsCorrect) {
        nCorrectChoices += 1;
      }
      if (choiceIsCorrect && isChecked) {
        nCorrect += 1;
      }
      if (!choiceIsCorrect && isChecked) {
        nWrong += 1;
      }
    }

    let maxPoints = getAttribute(exam, "points");
    let points = (
      (Math.max(0, nCorrect - nWrong) / nCorrectChoices) *
      parseInt(maxPoints)
    ).toFixed(2);

    let score = exam.querySelector("div.score span.display");
    score.textContent = points + " / " + maxPoints;
  }
}

function gradeFF(exam) {
  let score = exam.querySelector("div.score span.display");
  score.textContent += "0 / 0";
}

function gradeNU(exam) {
  let nuAnswer = exam.querySelector("div.solution div.correct");
  let score = exam.querySelector("div.score span.display");
  score.textContent += "0 / 0";
}

function gradeMA(exam) {
  let maAnswer = exam.querySelector("table.exa-ma");
  if (maAnswer !== null) {
    let details = maAnswer.querySelectorAll("tr.detail");
    let nDetails = details.length;
    let nCorrect = 0;
    let nWrong = 0;
    for (let detail of details) {
      let select = detail.querySelector("select");
      let selectedOption = select.value;
      let correctOption = getAttribute(detail, "correct");

      if (selectedOption === correctOption) {
        nCorrect += 1;
      } else {
        nWrong += 1;
      }
    }

    let score = exam.querySelector("div.score span.display");
    let maxPoints = getAttribute(exam, "points");

    let points = ((nCorrect / nDetails) * parseInt(maxPoints)).toFixed(2);

    score.textContent = points + " / " + maxPoints;
  }
}

function checkChoice(choice, value) {
  let check = choice.querySelector("div.check-box");
  if (value) setAttribute(check, "checked", true);
  else removeAttribute(check, "checked");
}

function addCheckBox(exam, choice) {
  let choiceIsCorrect = choice.classList.contains("correct");
  let check = choice.querySelector("div.check-box");
  let mark = document.createElement("i");
  if (choiceIsCorrect) mark.classList.add("fas", "fa-check", "correct");
  else mark.classList.add("fas", "fa-times", "wrong");
  let button = document.createElement("span");
  let checked = document.createElement("i");
  checked.classList.add("far", "fa-check-square", "checked");
  let unchecked = document.createElement("i");
  unchecked.classList.add("far", "fa-square", "unchecked");
  button.appendChild(checked);
  button.appendChild(unchecked);
  check.appendChild(mark);
  check.appendChild(button);
  button.addEventListener("click", (e) => {
    if (!hasAttribute(exam, "solved")) {
      toggleAttribute(check, "checked");
    }
  });
}

function prepareExaminer() {
  let exams = document.querySelectorAll("div.exa-quest");
  console.log("questions:", exams.length);
  for (let exam of exams) {
    let poll = exam.classList.contains("poll");
    let solve = exam.querySelector("button.solve");
    let again = exam.querySelector("button.again");

    // Multiple choice answer
    let mcAnswer = exam.querySelector("div.exa-mc");
    if (mcAnswer !== null) {
      if (!poll) shuffleChildren(mcAnswer);
      let choices = mcAnswer.querySelectorAll("div.exa-mc div.choice");
      console.log("choices:", choices.length);
      for (let choice of choices) {
        addCheckBox(exam, choice);
      }
      solve.addEventListener("click", (_) => {
        setAttribute(exam, "solved", true);
        gradeMC(exam);
      });
      again.addEventListener("click", (_) => {
        removeAttribute(exam, "solved");
        for (let choice of choices) {
          checkChoice(choice, false);
        }
        if (!poll) shuffleChildren(mcAnswer);
      });
    }

    // Free form answer
    let ffAnswer = exam.querySelector("div.exa-ff");
    if (ffAnswer !== null) {
      let textarea = ffAnswer.querySelector("textarea");
      solve.addEventListener("click", (_) => {
        gradeFF(exam);
        setAttribute(exam, "solved", true);
        textarea.setAttribute("readonly", true);
      });
      again.addEventListener("click", (_) => {
        removeAttribute(exam, "solved");
        textarea.removeAttribute("readonly");
        textarea.value = "";
      });
    }

    // Free form answer
    let nuAnswer = exam.querySelector("div.exa-nu");
    if (nuAnswer !== null) {
      let textarea = nuAnswer.querySelector("textarea");
      solve.addEventListener("click", (_) => {
        gradeNU(exam);
        setAttribute(exam, "solved", true);
        textarea.setAttribute("readonly", true);
      });
      again.addEventListener("click", (_) => {
        removeAttribute(exam, "solved");
        textarea.removeAttribute("readonly");
        textarea.value = "";
      });
    }

    // Multiple answers
    let maAnswer = exam.querySelector("table.exa-ma");
    if (maAnswer !== null) {
      // shuffleChildren(maAnswer.firstChild);
      let rows = exam.querySelectorAll("table.exa-ma tr.detail");
      for (let row of rows) {
        let result = row.querySelector("td.result");
        let correct = document.createElement("i");
        correct.classList.add("fas", "fa-check", "correct");
        result.appendChild(correct);
        let wrong = document.createElement("i");
        wrong.classList.add("fas", "fa-times", "wrong");
        result.appendChild(wrong);
      }
      solve.addEventListener("click", (_) => {
        gradeMA(exam);
        setAttribute(exam, "solved", true);
        for (let row of rows) {
          let select = row.querySelector("select");
          let correct = getAttribute(row, "correct");
          if (select.value === correct) {
            setAttribute(row, "right", true);
          } else {
            setAttribute(row, "wrong", true);
          }
          select.setAttribute("disabled", true);
        }
      });
      again.addEventListener("click", (_) => {
        removeAttribute(exam, "solved");
        for (let row of rows) {
          let select = row.querySelector("select");
          select.selectedIndex = 0;
          select.removeAttribute("disabled");
          removeAttribute(row, "right");
          removeAttribute(row, "wrong");
        }
        // shuffleChildren(maAnswer.firstChild);
      });
    }
  }
}

async function preparePolls(reveal) {
  // Only when reveal is ready we can find the elements we need.
  let color = Decker.meta['css-light-colors'];
  let session = await pollSession({
    serverUrl: Decker.meta["poll-server"],
    clientCss: `
    html {
      color: ${color.shade7};
      background-color: ${color.shade0};
    }
    h1#pollid {display: none}
    p#nvotes {display: none}
    body.polling p#status {display: none}
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
    `,
  });

  let { id, url } = session.sessionId();

  let revealSlide = document.querySelector("div.reveal.slide");
  let qrcodeContainer = document.createElement("div");

  qrcodeContainer.classList.add("qrcode", "container");
  qrcodeContainer.innerHTML = String.raw`
    <canvas id="poll-qrcode-canvas"></canvas>
    <span><a href="${url}" target="_blank" title="${url}" id="poll-session-id">${id}</a> </span>
  `;
  qrcodeContainer.addEventListener("click", () => {
    qrcodeContainer.classList.remove("show");
  });
  revealSlide.appendChild(qrcodeContainer);

  session.fillQRCode("poll-qrcode-canvas");

  // Prep all questions in the document marked with class 'poll'.
  let questions = document.querySelectorAll("div.question.poll");
  for (let question of questions) {
    // Only consider multiple choice for now
    let answer = question.querySelector("div.answer.exa-mc");
    let nvotes = parseInt(answer.getAttribute("data-votes")) || 1;
    if (!answer) return;

    // Wir the button for the QR Code.
    let qrcodeButton = question.querySelector("button.qrcode");
    qrcodeButton.addEventListener("click", () => {
      qrcodeContainer.classList.add("show");
    });

    // Collect the vote feedback structures.
    let voteBlocks = answer.querySelectorAll("div.choice div.vote");
    let voteMap = {};
    for (let vb of voteBlocks) {
      let choice = vb.getAttribute("label");
      let votesElement = vb.querySelector("div.votes");
      voteMap[choice] = votesElement;
    }
    let choices = Object.keys(voteMap);

    // The buttons
    let pollButton = question.querySelector("button.poll");
    let stopButton = question.querySelector("button.stop");
    stopButton.setAttribute("disabled", true);
    pollButton.removeAttribute("disabled");

    let abort = () => {
      console.log("Poll: aborted.");
      session.stop();
      session.reset();
    };

    // Wire the Poll button that starts the poll.
    pollButton.addEventListener("click", (e) => {
      session.poll(choices, nvotes, {
        onActive: (participants, votes) => {
          console.log("Poll:", "active", participants, votes);
          displayVotes(votes, voteMap);
          showVotes(voteBlocks, true);
          const voted = Object.values(votes).reduce((t, v) => t + v, 0);
          stopButton.textContent = `${voted} / ${participants}`;
          stopButton.removeAttribute("disabled");
          pollButton.setAttribute("disabled", false);
          reveal.on("slidechanged", abort);
        },
        onFinished: (participants, votes) => {
          console.log("Poll:", "finished", participants, votes);
          displayVotes(votes, voteMap);
          showVotes(voteBlocks, false);
          const voted = Object.values(votes).reduce((t, v) => t + v, 0);
          stopButton.textContent = `${voted} / ${participants}`;
          stopButton.setAttribute("disabled", false);
          pollButton.removeAttribute("disabled");
          reveal.off("slidechanged", abort);
        },
      });
    });

    // Wire the Stop button.
    stopButton.addEventListener("click", (e) => {
      session.stop();
    });
  }
}

// const totalVotes = Object.values(votes).reduce((t, v) => t + v, 0);

function showVotes(voteMap, show) {
  for (let vote of Object.values(voteMap))
    if (show) vote.classList.add("polling");
    else vote.classList.remove("polling");
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
    console.log(label, maxVotes, percent);
  }
}

function getAttribute(e, attrName) {
  return e.getAttribute("data-" + attrName);
}

function hasAttribute(e, attrName) {
  return e.getAttribute("data-" + attrName) !== null;
}

function setAttribute(e, attrName, value) {
  e.setAttribute("data-" + attrName, value);
}

function removeAttribute(e, attrName) {
  e.removeAttribute("data-" + attrName);
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

function shuffleChildren(node) {
  let array = [];
  while (node.firstChild) array.push(node.removeChild(node.firstChild));
  shuffle(array);
  for (let child of array) node.appendChild(child);
}

function shuffle(array) {
  var currentIndex = array.length,
    temporaryValue,
    randomIndex;
  while (0 !== currentIndex) {
    randomIndex = Math.floor(Math.random() * currentIndex);
    currentIndex -= 1;

    temporaryValue = array[currentIndex];
    array[currentIndex] = array[randomIndex];
    array[randomIndex] = temporaryValue;
  }
  return array;
}
