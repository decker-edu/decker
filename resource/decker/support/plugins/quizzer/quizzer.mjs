import Client from "./client.mjs";
import Renderer, { resetAssignmentState } from "./renderer.mjs";
import bwip from "../examiner/bwip.js";
import "../../vendor/d3.v6.min.js";
import localization from "./localization.mjs";
const l10n = localization();

/* Module Variables */
let Reveal;

let hostClient = undefined;
let activeQuiz = undefined;
let awaitingQuiz = undefined;
let resultsAvailable = false;

/*
 * UI Elements, pre initialized
 */

/* Bottom Row Buttons and Indicator */

let connectionIndicator = document.createElement("span");
let pollButton = document.createElement("button");
let tallySpan = document.createElement("span");

/* All the QR */

let qrDialog = document.createElement("dialog");
let qrCanvas = document.createElement("canvas");
let qrLeftCanvas = document.createElement("canvas");
let qrRightCanvas = document.createElement("canvas");
let qrLeftLabel = document.createElement("span");
let qrRightLabel = document.createElement("span");
let qrLink = document.createElement("a");
let qrClose = document.createElement("button");

/* Results */

let resultDialog = document.createElement("dialog");
let resultContainer = document.createElement("div");
let closeHint = document.createElement("span");

/**
 * Checks if the given rect contains the given (x,y) coordinate.
 * Used for checking if the QR Dialog itself is clicked or its backdrop.
 *
 * @param {*} rect
 * @param {*} x
 * @param {*} y
 * @returns
 */
function containsClick(rect, x, y) {
  return (
    rect.x <= x &&
    x <= rect.x + rect.width &&
    rect.y <= y &&
    y <= rect.y + rect.height
  );
}

/* Implementation of the Fisher-Yates Shuffle */
function shuffle(array) {
  let current = array.length;
  while (current != 0) {
    let random = Math.floor(Math.random() * current);
    current--;
    [array[current], array[random]] = [array[random], array[current]];
  }
}

/**
 * Parse out a JSON object representing the quiz out of the DOM of a .quizzer div.
 *
 * A quiz object has the following attributes:
 * - type: A string that represents the type of quiz (selection, choice, assignment or freetext)
 * - question: The question posed in the quiz to be presented to the questionee.
 * - choices: An array of objects representing the possible answers to a question.
 *
 * A choice contains the following attributes:
 * - votes: The amount of items one is allowed to pick at once (only relevant for choice quizzes)
 * - options: An array of objects each representing an answer.
 * - categories: Assignment quizzes also get this field to represent the categories.
 *
 * An answer contains the following attributes:
 * - label: The text representing the answer.
 * - reason: An optional reason given for why the answer is correct or incorrect. In
 *   assignment quizzes the reason represents the category an object is assigned to.
 * - correct: A boolean that represents if this specific answer is a correct answer or not.
 *
 * An example quiz would be represented as:
 * ```
 * ::: {.quizzer .[type]}
 *
 * Question
 *
 * - [ ] Wrong Answer
 *   - Reason for Wrongness
 * - [X] Correct Answer
 *   - Reason for Correctness
 *
 * :::
 * ```
 *
 * The question text can and should contain placeholders in the form of [#n] for freetext and
 * selection quizzes where the values of answers are inserted or where selection boxes should
 * be placed. If there are less placeholders than question groups they are appended at the bottom of the question.
 * Two lists of possible answers have to be separated by a horizontal rule (---), as
 * the lists will be compacted into one if there are only empty lines separating them.
 *
 * Because other types of separators might be content, rules (---) have been chosen to be removed from the container.
 */
function parseQuizzes(reveal) {
  const slides = reveal.getSlides();
  /* For each slide of reveal ... */
  for (const slide of slides) {
    const quizzers = slide.querySelectorAll(":scope .quizzer");
    /* ... check if there are more than one quiz on the slide and if so, replace the content of the slide with an error message ... */
    if (quizzers.length > 1) {
      const layout = slide.querySelector(":scope .layout");
      layout.innerHTML = `<div class="area">
  <div class="box block">
    <div class="quizzer">
      <div class="quizzer-container error">
        <div class="question-container">
         <p>${l10n.errorMultipleQuizzes}</p>
        </div>
      </div>
    </div>
  </div>
</div>`;
      continue;
    }
    /* ... for the single quizzer on the slide ... */
    for (const quizzer of quizzers) {
      const quizObject = {
        type: undefined,
        question: undefined,
        choices: [],
      };
      if (quizzer.classList.contains("assignment")) {
        quizObject.type = "assignment";
      } else if (quizzer.classList.contains("freetext")) {
        quizObject.type = "freetext";
      } else if (quizzer.classList.contains("selection")) {
        quizObject.type = "selection";
      } else {
        quizObject.type = "choice";
      }
      /* ... interpret each list in the container as a choice object ... */
      const lists = quizzer.querySelectorAll(":scope > ul.task-list");
      for (const list of lists) {
        const choiceObject = {
          votes: 1, // By default you have at least one vote
          options: [],
        };
        /* ... where each list item is a possible answer ... */
        const items = list.querySelectorAll(":scope > li");
        for (const item of items) {
          const answerObject = {
            label: undefined,
            reason: undefined,
            correct: false,
          };
          /* ... with an optional reason written as a nested list item ... */
          const reason = item.querySelector(":scope ul li");
          if (reason) {
            answerObject.reason = reason.innerHTML;
            reason.parentElement.remove();
          }
          /* ... cleanup ... */
          const checkbox = item.querySelector(":scope input[type='checkbox']");
          if (checkbox) {
            checkbox.remove();
          }
          /* ... add the the html of the item as its label (without the checkbox) ... */
          const label = item.innerHTML;
          answerObject.label = label;
          /* ... and set its correctness based on if it was cheked or not. */
          answerObject.correct = item.classList.contains("task-yes");
          choiceObject.options.push(answerObject);
        }
        /* ... if there is more than one correct choice the amount of votes is equal to the amount of possible answers. */
        if (
          choiceObject.options.filter((choice) => choice.correct).length > 1
        ) {
          choiceObject.votes = choiceObject.options.length;
        }
        quizObject.choices.push(choiceObject);
        /* ... remove the list from the DOM ... */
        list.remove();
      }
      /* ... remove hr elements from DOM because they are only used as list separators ... */
      const hrules = quizzer.querySelectorAll("hr");
      for (const hrule of hrules) {
        hrule.remove();
      }
      /* ... clean up empty spans and ps ... */
      while (
        quizzer.querySelectorAll(":is(span,p)[display]:empty").length > 0
      ) {
        const empties = quizzer.querySelectorAll(":is(p,span):empty");
        for (const empty of empties) {
          empty.remove();
        }
      }
      /* ... after parsing the answers, interpret the rest of the inner quiz as the question ... */
      quizObject.question = quizzer.innerHTML.trim();
      /* Clean up the entire quizzer container */
      while (quizzer.lastElementChild) {
        quizzer.lastElementChild.remove();
      }
      /* Refine the quiz object for network */
      for (const choice of quizObject.choices) {
        /* If the quiz is an assignment quiz the quiz needs categories */
        if (quizObject.type === "assignment") {
          let categories = [];
          for (const option of choice.options) {
            if (option.reason) {
              categories.push(option.reason);
            }
          }
          /* Remove duplicate categories: We only need one of each. */
          const uniqueCategories = categories.filter(
            (value, index, array) => array.indexOf(value) === index
          );
          const categoryObjects = [];
          /* Each category gets a number. */
          let number = 1;
          for (const category of uniqueCategories) {
            const object = {
              label: category,
              number: number++,
            };
            categoryObjects.push(object);
          }
          choice.categories = categoryObjects;
          /* Shuffle the assignable objects so each object is not in order of their corresponding catagoery. */
          shuffle(choice.options);
        }
        /* Assign a letter to each answer. */
        let letter = "A";
        for (const option of choice.options) {
          if (choice.categories) {
            const category = choice.categories.find(
              (category) => category.label === option.reason
            );
            if (category) {
              option.reason = category.number;
            } else {
              option.reason = 0;
            }
          }
          option.letter = letter;
          letter = String.fromCharCode(letter.charCodeAt(0) + 1);
        }
      }
      /* Archive the quiz object into the quizzer container */
      quizzer.quiz = quizObject;
      /* Render the quiz interface according to its type */
      if (quizObject.type === "choice") {
        Renderer.renderChoiceQuiz(quizzer, quizObject);
      } else if (quizObject.type === "freetext") {
        Renderer.renderFreeTextQuiz(quizzer, quizObject);
      } else if (quizObject.type === "selection") {
        Renderer.renderSelectionQuiz(quizzer, quizObject);
      } else if (quizObject.type === "assignment") {
        Renderer.renderAssignmentQuiz(quizzer, quizObject);
      }
      /* Also archive the quiz object in the slide as to easily identify slides that have quizzes */
      slide.quiz = quizObject;
    }
  }
}

/* Assigns classes and hierarchy to every UI element and places them in the DOM */
function createHostInterface(reveal) {
  /* Without the bottom button anchor we quit. */
  if (!reveal.hasPlugin("ui-anchors")) {
    return;
  }
  const anchors = reveal.getPlugin("ui-anchors");

  /* Poll Button */
  pollButton.classList.add(
    "quizzer-button",
    "fas",
    "fa-poll",
    "fa-button",
    "presenter-only",
    "quiz-only"
  );

  /* The "fullscreen" QR Dialog */
  qrDialog.classList.add("quizzer-dialog");
  qrDialog.addEventListener("click", (event) => {
    if (
      event.target &&
      event.target.classList.contains("close-dialog-button")
    ) {
      return;
    }
    if (!containsClick(qrDialog.getBoundingClientRect(), event.x, event.y)) {
      qrDialog.close();
    }
  });
  qrClose.classList.add("fas", "fa-times", "fa-button", "close-dialog-button");
  qrClose.addEventListener("click", (event) => {
    qrDialog.close();
  });

  qrDialog.appendChild(qrCanvas);
  qrDialog.appendChild(qrLink);
  qrDialog.appendChild(qrClose);

  /* The left and right QR codes, visible when a poll is active */
  qrLeftLabel.className = "hint";
  qrRightLabel.className = "hint";
  const leftWrapper = document.createElement("div");
  leftWrapper.className = "side-canvas-wrapper left-canvas";
  leftWrapper.appendChild(qrLeftCanvas);
  leftWrapper.appendChild(qrLeftLabel);
  const rightWrapper = document.createElement("div");
  rightWrapper.className = "side-canvas-wrapper right-canvas";
  rightWrapper.appendChild(qrRightCanvas);
  rightWrapper.appendChild(qrRightLabel);
  const sideCanvasWrappers = [leftWrapper, rightWrapper];
  for (const wrapper of sideCanvasWrappers) {
    wrapper.addEventListener("click", () => {
      qrDialog.showModal();
    });
    document.body.appendChild(wrapper);
  }
  document.body.appendChild(qrDialog);

  /**
   * Poll Button Functionality:
   * First Click: Start Quiz
   * Second Click: Request Evaluation
   */
  pollButton.addEventListener("click", async (event) => {
    /* Do nothing if disabled (custom handling because the disabled attribute is considered confusing) */
    if (pollButton.hasAttribute("aria-disabled")) {
      return;
    }
    requireHost((host) => {
      const slide = Reveal.getCurrentSlide();
      if (resultsAvailable) {
        showResults();
      } else if (activeQuiz) {
        host.requestEvaluation();
        document.documentElement.classList.remove("active-poll");
        awaitingQuiz = activeQuiz;
        activeQuiz = null;
      } else if (slide && slide.quiz) {
        activeQuiz = slide.quiz;
        document.documentElement.classList.add("active-poll");
        pollButton.title = l10n.evaluate;
        pollButton.ariaLabel = l10n.evaluate;
        host.sendQuiz(activeQuiz);
      }
    });
  });
  pollButton.title = l10n.activatePoll;

  /* Span for ( DONE/USERS ) display */
  tallySpan.className =
    "quizzer-state-info presenter-only quiz-only during-poll";

  connectionIndicator.classList.add(
    "fas",
    "fa-wifi",
    "fa-span",
    "presenter-only",
    "quiz-only"
  );
  connectionIndicator.title = l10n.uninitialized;
  connectionIndicator.ariaLabel = l10n.uninitialized;

  closeHint.innerText = l10n.clickToClose;
  closeHint.className = "close-hint";
  resultContainer.classList.add("quizzer-results-container");

  resultDialog.classList.add("quizzer-result-dialog");
  resultDialog.appendChild(closeHint);
  resultDialog.appendChild(resultContainer);
  resultDialog.addEventListener("click", hideResults);
  document.body.appendChild(resultDialog);

  /* Finish by placing buttons in the UI */
  anchors.placeButton(connectionIndicator, "BOTTOM_CENTER");
  anchors.placeButton(pollButton, "BOTTOM_CENTER");
  anchors.placeButton(tallySpan, "BOTTOM_CENTER");
}

/**
 * Change the connection indicator to an error state and displays a message on it.
 */
function displayConnectionError(message) {
  connectionIndicator.classList.remove("ok");
  connectionIndicator.classList.add("error");
  connectionIndicator.title = message;
  connectionIndicator.ariaLabel = message;
  pollButton.setAttribute("aria-disabled", true);
}

/**
 * Error handler of errors emitted by the client.
 * @param {*} error The error message
 * @param {*} details The details assigned to the error (if any)
 */
function onError(error, details) {
  console.error(error, details);
  if (error === "disconnect") {
    // We lost an active connection to the server. We may reconnect.
    displayConnectionError(l10n.disconnected);
  } else if (error === "connect") {
    // We were unable to connect to the server at all. We are probably offline.
    displayConnectionError(l10n.unableToConnect);
  } else if (error === "get session") {
    // The server refused our request to a new session. This should never happen.
    displayConnectionError(l10n.unableToGetSession);
  } else if (error === "session lost") {
    // If we lost the session the server has probably reset somehow.
    // We need a new session, so we just create a new client.
    displayConnectionError(l10n.sessionLost);
    hostClient.destroy();
    hostClient = null;
  } else if (error === "server error") {
    // The socket reported some error
    displayConnectionError(l10n.unknownError);
  }
}

/**
 * Event handler for updates to the participant list.
 * @param {*} connections Active connections.
 * @param {*} done Given answers.
 */
function onParticipants(connections, done) {
  tallySpan.innerText = `${done} / ${connections}`;
}

/**
 * Event handler for pongs from dedicated pings.
 * Socket.IO pings once every 25s, but the host initiates its own ping
 * to have an estimate of latency every second.
 * This should also result in quicker detection of connection errors.
 * @param {*} ms
 */
function onPong(ms) {
  connectionIndicator.classList.remove("error");
  connectionIndicator.classList.add("ok");
  const slide = Reveal.getCurrentSlide();
  if (slide.quiz) {
    document.documentElement.classList.add("quiz-available");
  }
  connectionIndicator.title = `${l10n.latency}${ms}ms`;
  connectionIndicator.ariaLabel = `${l10n.latency}${ms}ms`;
  pollButton.removeAttribute("aria-disabled");
}

const hostCallbacks = [];

/**
 * Creates a client object that hosts a quiz session.
 * If it needs to be created, calls the callback function once the host is ready.
 * If it already exists, simply calls the callback immediatly.
 * @param {*} callback Function to be executed once the host has been created.
 */
function requireHost(callback) {
  if (!hostClient) {
    if (callback) hostCallbacks.push(callback);
    hostClient = new Client();
    window.Quizzer = hostClient;

    hostClient.on("error", onError);

    hostClient.on("participants", onParticipants);

    hostClient.on("pong", onPong);

    hostClient.on("result", renderResult);

    hostClient.on("ready", (session, secret) => {
      let backend = Decker.meta.quizzer?.url || "http://localhost:3000/";
      if (backend.slice(-1) !== "/") {
        backend = backend + "/";
      }
      bwip.toCanvas(qrCanvas, {
        bcid: "qrcode",
        text: `${backend}${session}`,
        scale: 12,
        includetext: true,
        textxalign: "center",
        eclevel: "L",
      });
      const sideCanvases = [qrLeftCanvas, qrRightCanvas];
      for (const sideCanvas of sideCanvases) {
        bwip.toCanvas(sideCanvas, {
          bcid: "qrcode",
          text: `${backend}${session}`,
          scale: 4,
          includetext: true,
          textxalign: "center",
          eclevel: "L",
        });
        sideCanvas.dataset["session"] = session;
      }
      qrLeftLabel.innerText = session;
      qrRightLabel.innerText = session;
      qrLink.innerHTML = `${backend}&nbsp;&nbsp;&nbsp;→&nbsp;&nbsp;&nbsp;ID: ${session}`;
      qrLink.target = "_blank";
      qrLink.href = `${backend}${session}`;
      while (hostCallbacks.length > 0) {
        const callback = hostCallbacks.shift();
        callback();
      }
    });
  } else {
    if (callback) callback(hostClient);
  }
}

/**
 * Creates the result container and displays the graphs and diagrams.
 * TODO: Send the quiz type with the result.
 *
 * @param {*} result The result to be rendered. Right now the quiz and result need to match.
 * @returns
 */
function displayResult(result) {
  const resultContainer = document.createElement("div");

  // close on slide change
  Reveal.addEventListener("slidechanged", () => {
    resultContainer.remove();
  });

  // close button
  const closeButton = document.createElement("button");
  closeButton.title = l10n.clickToClose;
  closeButton.className = "close-button fa-button fas fa-times-circle";
  resultContainer.appendChild(closeButton);
  closeButton.addEventListener("click", () => {
    resultContainer.remove();
  });

  // handle mouse translation
  resultContainer.dragging = false;
  resultContainer.dx = 0.0;
  resultContainer.dy = 0.0;
  resultContainer.onmousedown = (e) => {
    const x = e.offsetX;
    const y = e.offsetY;
    const w = resultContainer.clientWidth;
    const h = resultContainer.clientHeight;
    const o = 20;
    if (x < w - o && y < h - o) {
      resultContainer.dragging = true;
      resultContainer.style.cursor = "move";
      resultContainer.lastX = e.screenX;
      resultContainer.lastY = e.screenY;
    }
  };
  resultContainer.onmousemove = (e) => {
    if (resultContainer.dragging) {
      const x = e.screenX;
      const y = e.screenY;
      resultContainer.dx += x - resultContainer.lastX;
      resultContainer.dy += y - resultContainer.lastY;
      resultContainer.lastX = x;
      resultContainer.lastY = y;
      resultContainer.style.translate = `${resultContainer.dx}px ${resultContainer.dy}px`;
    }
  };
  resultContainer.onmouseup = (e) => {
    resultContainer.style.cursor = "inherit";
    resultContainer.dragging = false;
  };

  resultContainer.classList.add("quizzer-results-container");
  if (awaitingQuiz && awaitingQuiz.type === "choice") {
    const entryContainer = document.createElement("div");
    entryContainer.classList.add("quizzer-result");
    const canvas = document.createElement("canvas");
    entryContainer.appendChild(canvas);
    resultContainer.appendChild(entryContainer);
    const labels = [];
    const values = [];
    for (const entry of result.items) {
      labels.push(entry.letter);
      values.push(entry.chosen);
    }
    const total = values.reduce((a, b) => a + b, 0);
    const data = values.map((a) => a / total);
    let context = canvas.getContext("2d");
    const chart = new Chart(context, {
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
  if (awaitingQuiz && awaitingQuiz.type === "freetext") {
    for (const words of result.items) {
      const entryContainer = document.createElement("div");
      entryContainer.classList.add("quizzer-result");
      const canvas = document.createElement("canvas");
      canvas.width = 1024;
      canvas.height = 512;
      entryContainer.appendChild(canvas);
      resultContainer.appendChild(entryContainer);
      const array = [];
      let most = 0;
      for (const entry of words) {
        array.push([entry.text, entry.count]);
        most = most < entry.count ? entry.count : most;
      }
      WordCloud(canvas, {
        list: array,
        gridSize: 4,
        weightFactor: (size) => {
          return (size / most) * 64;
        },
      });
    }
  }
  if (awaitingQuiz && awaitingQuiz.type === "selection") {
    for (const selection of result.items) {
      const entryContainer = document.createElement("div");
      entryContainer.classList.add("quizzer-result");
      const canvas = document.createElement("canvas");
      entryContainer.appendChild(canvas);
      resultContainer.appendChild(entryContainer);
      const labels = [];
      const values = [];
      for (const item of selection) {
        labels.push(item.text);
        values.push(item.count);
      }
      const total = values.reduce((a, b) => a + b, 0);
      const data = values.map((a) => a / total);
      let context = canvas.getContext("2d");
      const chart = new Chart(context, {
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
  }
  /**
   * Creates the assignment quiz sankey diagram. The code is very ugly because the creation of the diagram
   * is extremely fiddly with d3sankey.
   */
  if (awaitingQuiz && awaitingQuiz.type === "assignment") {
    let total = 0;
    for (const assignment of result.assignments) {
      total += assignment.count;
    }
    if (total === 0) {
      return;
    }
    const margin = { top: 16, right: 16, left: 16, bottom: 16 };
    const width = 800 - margin.left - margin.right;
    const height = 600 - margin.top - margin.bottom;
    const color = d3.scaleOrdinal(d3.schemeCategory10);
    const generator = d3
      .sankey()
      .nodeWidth(24)
      .nodePadding(32)
      .size([width, height]);
    const svg = d3
      .select(resultContainer)
      .append("svg")
      .attr("class", "quizzer-result")
      .attr("width", width + margin.left + margin.right)
      .attr("height", height + margin.top + margin.bottom)
      .append("g")
      .attr("transform", "translate(" + margin.left + "," + margin.top + ")");
    const nodes = [];
    const links = [];
    const choice = awaitingQuiz.choices[0];
    const options = choice.options;
    const categories = choice.categories;
    for (const option of options) {
      const node = {
        node: nodes.length,
        name: option.letter,
      };
      nodes.push(node);
    }
    for (const category of categories) {
      const node = {
        node: nodes.length,
        name: String(category.number),
      };
      nodes.push(node);
    }
    nodes.push({ node: nodes.length, name: "0" });
    for (const assignment of result.assignments) {
      const source = nodes.find((node) => node.name === assignment.letter);
      const target = nodes.find(
        (node) => node.name === String(assignment.number)
      );
      if (source && target) {
        const link = {
          source: source.node,
          target: target.node,
          value: assignment.count,
        };
        links.push(link);
      }
    }
    const json = { nodes: nodes, links: links };
    generator.extent([
      [16, 16],
      [width - 32, height - 32],
    ]);
    var graph = generator(json);
    var link = svg
      .append("g")
      .selectAll(".link")
      .data(graph.links)
      .enter()
      .append("path")
      .attr("class", "link")
      .attr("d", d3.sankeyLinkHorizontal())
      .style("stroke-width", (d) => d.width)
      .sort(function (a, b) {
        return b.y1 - b.y0 - (a.y1 - a.y0);
      });

    // add in the nodes
    var node = svg
      .append("g")
      .selectAll(".node")
      .data(graph.nodes)
      .enter()
      .append("g")
      .attr("class", "node")
      .call(
        d3
          .drag()
          .subject(function (d) {
            return d;
          })
          .on("start", function () {
            this.parentNode.appendChild(this);
          })
          .on("drag", dragmove)
      );

    // add the rectangles for the nodes
    node
      .append("rect")
      .attr("x", (d) => d.x0)
      .attr("y", (d) => d.y0)
      .attr("height", (d) => d.y1 - d.y0)
      .attr("width", generator.nodeWidth())
      .style("fill", function (d) {
        return (d.color = color(d.name.replace(/ .*/, "")));
      })
      .style("stroke", function (d) {
        return d3.rgb(d.color).darker(2);
      })
      // Add hover text
      .append("title")
      .text(function (d) {
        return d.name + "\n" + d.value;
      });

    // add in the title for the nodes
    node
      .append("text")
      .attr("x", (d) => {
        if (!isNaN(parseInt(d.name))) {
          return d.x1 + 32;
        } else {
          return d.x1 - 32;
        }
      })
      .attr("y", function (d) {
        return d.y0 + (d.y1 - d.y0) / 2;
      })
      .attr("dy", ".35em")
      .attr("text-anchor", "end")
      .attr("transform", null)
      .text(function (d) {
        return d.name === "0" ? "N/A" : d.name;
      })
      .filter(function (d) {
        return d.x < width / 2;
      })
      .attr("x", 6 + generator.nodeWidth())
      .attr("text-anchor", "start");

    // the function for moving the nodes
    function dragmove(d) {
      d3.select(this).attr(
        "transform",
        "translate(" +
          d.x +
          "," +
          (d.y = Math.max(0, Math.min(height - d.dy, e.y))) +
          ")"
      );
      generator.relayout();
      link.attr("d", d3.sankeyLinkHorizontal());
    }
  }
<<<<<<< HEAD
  resultsAvailable = true;
  document.documentElement.classList.add("results-available");
  showResults();
}

function showResults() {
  resultDialog.showModal();
}

function hideResults() {
  resultDialog.close();
=======

  document.body.appendChild(resultContainer);
>>>>>>> 6801e623f233e9d8485ae4a112d5fa031d0d0d3f
}

/**
 * Switches visibility of the quiz interface based on if the current slide
 * has a quiz. If presenter mode is off, all ui elements should have a
 * "presenter-only" class to hide them anyway.
 * Triggered by onSlideChanged or onPresenterMode
 * @returns
 */
async function toggleInterface() {
  const slide = Reveal.getCurrentSlide();
  /* No Quizzes on current slide: Hide interface. */
  if (!slide.quiz) {
    document.documentElement.classList.remove("quiz-available");
    return;
  }
  requireHost();
  document.documentElement.classList.add("quiz-available");
}

/**
 * When turning on presenter mode, check if there are quizzes available.
 * @param {*} active
 */
async function onPresenterMode(active) {
  if (active) {
    toggleInterface();
  }
}

/**
 * When the current slide changes, check if the new slide has a quiz.
 * Also end current quiz if there is one running.
 * @param {*} event
 * @returns
 */
async function onSlideChange(event) {
  resetAssignmentState();
  resultsAvailable = false;
  document.documentElement.classList.remove("results-available");
  pollButton.title = l10n.activatePoll;
  pollButton.ariaLabel = l10n.activatePoll;
  if (!Decker.isPresenterMode()) {
    return;
  }
  if (!event.currentSlide) {
    return;
  }
  requireHost((host) => {
    if (activeQuiz) {
      host.requestEvaluation();
      document.documentElement.classList.remove("active-poll");
      awaitingQuiz = activeQuiz;
      activeQuiz = null;
      tallySpan.innerText = "";
    }
  });
  toggleInterface();
}

const Plugin = {
  id: "quizzerPlugin",
  init: (reveal) => {
    if (reveal.isSpeakerNotes()) {
      return;
    }
    createHostInterface(reveal);
    parseQuizzes(reveal);
    reveal.on("ready", () => {
      reveal.on("slidechanged", onSlideChange);
      Decker.addPresenterModeListener(onPresenterMode);
    });
    Reveal = reveal;
  },
};

export default Plugin;
