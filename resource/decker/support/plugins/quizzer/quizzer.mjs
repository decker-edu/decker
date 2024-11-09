import Client from "./client.mjs";
import Renderer, { resetAssignmentState } from "./renderer.mjs";
import bwip from "../examiner/bwip.js";
import "../../vendor/d3.v6.min.js";
import localization from "./localization.mjs";

const l10n = localization();

let Reveal;
let hostClient = undefined;
let currentQuiz = undefined;
let quizState = "UNINITIALIZED";

let connectionIndicator = document.createElement("span");

//let qrButton = document.createElement("button");
let qrDialog = document.createElement("dialog");
let qrCanvas = document.createElement("canvas");
let qrLeftCanvas = document.createElement("canvas");
let qrRightCanvas = document.createElement("canvas");
let qrLeftLabel = document.createElement("span");
let qrRightLabel = document.createElement("span");
let qrLink = document.createElement("a");
let qrClose = document.createElement("button");

let pollButton = document.createElement("button");
let tallySpan = document.createElement("span");

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

function setQuizState(state) {
  document.documentElement.classList.remove("active-poll");
  quizState = state;
  tallySpan.hidden = true;
  delete pollButton.dataset["state"];
  if (state === "ACTIVE") {
    document.documentElement.classList.add("active-poll");
    tallySpan.hidden = false;
    pollButton.dataset["state"] = "ACTIVE";
    pollButton.title = l10n.evaluate;
    pollButton.ariaLabel = l10n.evaluate;
  } else if (state === "ERROR") {
    connectionIndicator.classList.add("show");
  }
}

/**
 * Parse out a JSON object representing the quiz out of the DOM of a .quizzer div.
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
 * be placed. Two lists of possible answers have to be separated by a horizontal rule (---), as
 * the lists will be compacted into one if there are only empty lines separating them.
 */
function parseQuizzes(reveal) {
  const slides = reveal.getSlides();
  for (const slide of slides) {
    const quizzers = slide.querySelectorAll(":scope .quizzer");
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
    /* For each quizzer div in the slide ... */
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
      const lists = quizzer.querySelectorAll(":scope > ul");
      for (const list of lists) {
        const choiceObject = {
          votes: 1,
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
        if (quizObject.type === "assignment") {
          let categories = [];
          for (const option of choice.options) {
            if (option.reason) {
              categories.push(option.reason);
            }
          }
          const uniqueCategories = categories.filter(
            (value, index, array) => array.indexOf(value) === index
          );
          const categoryObjects = [];
          let number = 1;
          for (const category of uniqueCategories) {
            const object = {
              label: category,
              number: number++,
            };
            categoryObjects.push(object);
          }
          choice.categories = categoryObjects;
          shuffle(choice.options);
        }
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

function createHostInterface(reveal) {
  if (!reveal.hasPlugin("ui-anchors")) {
    return;
  }

  const anchors = reveal.getPlugin("ui-anchors");
  /*  qrButton.classList.add(
    "fas",
    "fa-qrcode",
    "fa-button",
    "presenter-only",
    "during-poll",
    "quiz-only"
  );
  qrButton.id = "quizzer-qr-button"; */
  pollButton.classList.add(
    "quizzer-button",
    "fas",
    "fa-poll",
    "fa-button",
    "presenter-only",
    "quiz-only"
  );
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

  /* No more QR Code Button in the Interface
  qrButton.addEventListener("click", async (event) => {
    const root = document.documentElement;
    if (root.classList.contains("hide-qr")) {
      root.classList.add("hide-qr");
      qrButton.title = l10n.hideQRCode;
      qrButton.ariaLabel = l10n.hideQRCode;
    } else {
      root.classList.remove("hide-qr");
      qrButton.title = l10n.showQRCode;
      qrButton.ariaLabel = l10n.showQRCode;
    }
  });
  qrButton.title = l10n.hideQRCode;
  qrButton.ariaLabel = l10n.hideQRCode;
  */
  pollButton.addEventListener("click", async (event) => {
    if (!hostClient) {
      await initializeHost();
    }
    if (quizState === "READY") {
      const slide = Reveal.getCurrentSlide();
      if (slide.quiz) {
        currentQuiz = slide.quiz;
        hostClient.sendQuiz(currentQuiz);
      }
      setQuizState("ACTIVE");
    } else if (quizState === "ACTIVE") {
      hostClient.requestEvaluation();
      setQuizState("AWAITING_EVALUATION");
    }
  });
  pollButton.title = l10n.activatePoll;

  tallySpan.className = "quizzer-state-info presenter-only quiz-only";

  connectionIndicator.classList.add(
    "fas",
    "fa-wifi",
    "fa-span",
    "presenter-only",
    "quiz-only"
  );
  connectionIndicator.title = "No Data";

  anchors.placeButton(connectionIndicator, "BOTTOM_CENTER");
  //anchors.placeButton(qrButton, "BOTTOM_CENTER");
  anchors.placeButton(pollButton, "BOTTOM_CENTER");
  anchors.placeButton(tallySpan, "BOTTOM_CENTER");
}

function onError(error) {
  document.documentElement.classList.remove("quiz-available");
  connectionIndicator.classList.remove("ok");
  connectionIndicator.classList.add("error");
  if (error === "disconnect") {
    connectionIndicator.title = l10n.disconnected;
    connectionIndicator.ariaLabel = l10n.disconnected;
  } else if (error === "connect") {
    connectionIndicator.title = l10n.unableToConnect;
    connectionIndicator.ariaLabel = l10n.unableToConnect;
  } else if (error === "get session") {
    connectionIndicator.title = l10n.unableToGetSession;
    connectionIndicator.ariaLabel = l10n.unableToGetSession;
  } else if (error === "not host") {
    // wait
  } else if (error === "unknown") {
    connectionIndicator.title = l10n.unknownError;
    connectionIndicator.ariaLabel = l10n.unknownError;
  }
  setQuizState("ERROR");
}

function onStateUpdate(connections, done) {
  tallySpan.innerText = `${done} / ${connections}`;
}

function onConnectionUpdate(ms) {
  connectionIndicator.classList.remove("error");
  connectionIndicator.classList.add("ok");
  const slide = Reveal.getCurrentSlide();
  if (slide.quiz) {
    document.documentElement.classList.add("quiz-available");
  }
  connectionIndicator.title = `${l10n.latency}${ms}ms`;
  connectionIndicator.ariaLabel = `${l10n.latency}${ms}ms`;
}

async function initializeHost() {
  return new Promise((resolve, reject) => {
    hostClient = new Client();
    hostClient.on("error", onError);
    hostClient.on("state", onStateUpdate);
    hostClient.on("connection", onConnectionUpdate);
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
      qrLink.innerText = `${backend}${session}`;
      qrLink.target = "_blank";
      qrLink.href = `${backend}${session}`;
      setQuizState("READY");
      resolve();
    });
    hostClient.on("result", (result) => {
      console.log(result);
      quizState = "READY";
      tallySpan.hidden = true;
      const resultContainer = document.createElement("div");
      const closeHint = document.createElement("p");
      closeHint.innerText = l10n.clickToClose;
      closeHint.className = "close-hint";
      resultContainer.appendChild(closeHint);
      resultContainer.classList.add("quizzer-results-container");
      if (currentQuiz && currentQuiz.type === "choice") {
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
      if (currentQuiz && currentQuiz.type === "freetext") {
        for (const words of result.items) {
          const entryContainer = document.createElement("div");
          entryContainer.classList.add("quizzer-result");
          const canvas = document.createElement("canvas");
          canvas.width = 512;
          canvas.height = 256;
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
            gridSize: 8,
            weightFactor: (size) => {
              return (size / most) * 64;
            },
          });
        }
      }
      if (currentQuiz && currentQuiz.type === "selection") {
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
      if (currentQuiz && currentQuiz.type === "assignment") {
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
          .attr(
            "transform",
            "translate(" + margin.left + "," + margin.top + ")"
          );
        const nodes = [];
        const links = [];
        const choice = currentQuiz.choices[0];
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
        console.log(nodes);
        console.log(links);
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
      document.body.appendChild(resultContainer);
      resultContainer.addEventListener("click", () => {
        resultContainer.remove();
      });
    });
  });
}

async function toggleInterface() {
  const slide = Reveal.getCurrentSlide();
  /* No Quizzes on current slide: Hide interface. */
  if (!slide.quiz) {
    document.documentElement.classList.remove("quiz-available");
    return;
  }
  document.documentElement.classList.add("quiz-available");
  if (!hostClient) {
    try {
      await initializeHost();
    } catch (error) {
      return;
    }
  }
  if (quizState === "ACTIVE") {
    hostClient.requestEvaluation();
    setQuizState("AWAITING_EVALUATION");
  }
  tallySpan.innerText = "";
}

async function onPresenterMode(active) {
  if (active) {
    toggleInterface();
  }
}

async function onSlideChange(event) {
  resetAssignmentState();
  if (!Decker.isPresenterMode()) {
    return;
  }
  if (!event.currentSlide) {
    return;
  }
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
