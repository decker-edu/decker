import Client from "./client.mjs";
import Renderer, { resetAssignmentState } from "./renderer.mjs";
import bwip from "../examiner/bwip.js";
import "../../vendor/d3.v6.min.js";
import localization from "./localization.mjs";

let Reveal;
let hostClient = undefined;
let currentQuiz = undefined;
let quizState = "UNINITIALIZED";

let connectionIndicator = document.createElement("span");

let qrButton = document.createElement("button");
let qrDialog = document.createElement("dialog");
let qrCanvas = document.createElement("canvas");
let qrLink = document.createElement("a");
let qrClose = document.createElement("button");

let stateButton = document.createElement("button");
let tallySpan = document.createElement("span");

function containsClick(rect, x, y) {
  return (
    rect.x <= x &&
    x <= rect.x + rect.width &&
    rect.y <= y &&
    y <= rect.y + rect.height
  );
}

function setQuizState(state) {
  quizState = state;
  tallySpan.hidden = true;
  delete stateButton.dataset["state"];
  if (state === "WAITING") {
    stateButton.dataset["state"] = "WAITING";
  } else if (state === "ACTIVE") {
    tallySpan.hidden = false;
    stateButton.dataset["state"] = "ACTIVE";
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
 *
 * An answer contains the following attributes:
 * - label: The text representing the answer.
 * - reason: An optional reason given for why the answer is correct or incorrect. In
 *   assignment quizzes the reason represents the category an object is assigned to.
 * - correct: A boolean that represents if this specific answer is a correct answer or not.
 *
 * An example quiz would be represented as:
 * ```
 * ::: quizzer [type]
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
    slide.quizzes = [];
    const quizzers = slide.querySelectorAll(":scope .quizzer");
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
      const question = quizzer.querySelector(":scope p");
      /* ... interpret the first <p> as the quiz's question ... */
      if (question) {
        question.classList.add("question");
        quizObject.question = question.innerHTML;
      }
      /* ... and each list in the container as a choice object ... */
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
          const reason = item.querySelector(":scope ul li");
          if (reason) {
            answerObject.reason = reason.innerHTML;
            reason.parentElement.remove();
          }
          const checkbox = item.querySelector(":scope input[type='checkbox']");
          if (checkbox) {
            checkbox.remove();
          }
          const label = item.innerHTML;
          answerObject.label = label;
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
      }
      /* Clean up the entire quizzer container */
      while (quizzer.lastElementChild) {
        quizzer.lastElementChild.remove();
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
      slide.quizzes.push(quizObject);
    }
  }
}

function createHostInterface(reveal) {
  if (!reveal.hasPlugin("ui-anchors")) {
    return;
  }
  const anchors = reveal.getPlugin("ui-anchors");
  qrButton.hidden = true;
  qrButton.classList.add("fas", "fa-qrcode", "fa-button", "presenter-only");
  stateButton.hidden = true;
  stateButton.classList.add(
    "quizzer-button",
    "fas",
    "fa-poll",
    "fa-button",
    "presenter-only"
  );
  qrDialog.classList.add("quizzer-dialog");
  qrDialog.addEventListener("keyup", (event) => {
    if (event.key === "Escape") {
      qrDialog.close();
    }
  });
  qrDialog.addEventListener("click", (event) => {
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
  document.body.appendChild(qrDialog);

  qrButton.addEventListener("click", async (event) => {
    if (!hostClient) {
      await initializeHost();
    }
    qrDialog.showModal();
  });
  stateButton.addEventListener("click", async (event) => {
    if (!hostClient) {
      await initializeHost();
    }
    if (quizState === "READY") {
      const slide = Reveal.getCurrentSlide();
      if (slide.quizzes[0]) {
        currentQuiz = slide.quizzes[0];
        hostClient.sendQuiz(currentQuiz);
      }
      setQuizState("ACTIVE");
    } else if (quizState === "ACTIVE") {
      hostClient.requestEvaluation();
      setQuizState("AWAITING_EVALUATION");
    }
  });

  tallySpan.className = "quizzer-state-info";
  tallySpan.className = "presenter-only";

  connectionIndicator.classList.add(
    "fas",
    "fa-wifi",
    "fa-span",
    "presenter-only"
  );
  connectionIndicator.title = "No Data";
  connectionIndicator.hidden = true;

  anchors.placeButton(connectionIndicator, "BOTTOM_CENTER");
  anchors.placeButton(qrButton, "BOTTOM_CENTER");
  anchors.placeButton(stateButton, "BOTTOM_CENTER");
  anchors.placeButton(tallySpan, "BOTTOM_CENTER");
}

function onError() {
  console.error(
    "The Quizzer Websocket has encountered an error. The connection is to be considered closed."
  );
  connectionIndicator.classList.add("error");
  connectionIndicator.hidden = false;
  connectionIndicator.title = localization().unableToConnect;
  qrButton.hidden = true;
  stateButton.hidden = true;
  tallySpan.hidden = true;
  setQuizState("ERROR");
}

function onStateUpdate(connections, done) {
  tallySpan.innerText = `${done} / ${connections}`;
}

function onConnectionUpdate(error, outgoing, incoming) {
  connectionIndicator.classList.remove("error");
  if (error && error === "CLOSED") {
    connectionIndicator.title = "Connection Closed";
    connectionIndicator.classList.add("error");
    return;
  }
  connectionIndicator.title = `Out: ${Math.ceil(outgoing)}ms / In: ${Math.ceil(
    incoming
  )}ms`;
}

async function initializeHost() {
  return new Promise((resolve, reject) => {
    hostClient = new Client();
    hostClient.on("error", onError);
    hostClient.on("state", onStateUpdate);
    hostClient.on("connection", onConnectionUpdate);
    hostClient.on("ready", (session, secret) => {
      const backend = Decker.meta.quizzer.backend || "http://localhost:3000";
      bwip.toCanvas(qrCanvas, {
        bcid: "qrcode",
        text: `${backend}/client?session=${session}`,
        scale: 16,
        includetext: true,
        textxalign: "center",
        eclevel: "L",
      });
      qrLink.innerText = `${backend}/client?session=${session}`;
      qrLink.target = "_blank";
      qrLink.href = `${backend}/client?session=${session}`;
      setQuizState("READY");
      resolve();
    });
    hostClient.on("result", (result) => {
      quizState = "READY";
      tallySpan.hidden = true;
      const resultContainer = document.createElement("div");
      resultContainer.classList.add("quizzer-results-container");
      if (currentQuiz && currentQuiz.type === "choice") {
        const entryContainer = document.createElement("div");
        entryContainer.classList.add("quizzer-result");
        const canvas = document.createElement("canvas");
        entryContainer.appendChild(canvas);
        resultContainer.appendChild(entryContainer);
        const labels = [];
        const values = [];
        for (const label in result) {
          labels.push(label);
          values.push(result[label]);
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
        for (const words of result) {
          const entryContainer = document.createElement("div");
          entryContainer.classList.add("quizzer-result");
          const canvas = document.createElement("canvas");
          canvas.width = 512;
          canvas.height = 256;
          entryContainer.appendChild(canvas);
          resultContainer.appendChild(entryContainer);
          const array = [];
          let most = 0;
          for (const entry in words) {
            array.push([entry, words[entry]]);
            most = most < words[entry] ? words[entry] : most;
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
        for (const selection of result) {
          const entryContainer = document.createElement("div");
          entryContainer.classList.add("quizzer-result");
          const canvas = document.createElement("canvas");
          entryContainer.appendChild(canvas);
          resultContainer.appendChild(entryContainer);
          const labels = [];
          const values = [];
          for (const item in selection) {
            labels.push(item);
            values.push(selection[item]);
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
        const margin = { top: 16, right: 16, left: 16, bottom: 16 };
        const width = 800 - margin.left - margin.right;
        const height = 600 - margin.top - margin.bottom;
        const color = d3.scaleOrdinal(d3.schemeCategory10);
        const generator = d3
          .sankey()
          .nodeWidth(24)
          .nodePadding(32)
          .size([width, height]);
        console.log(generator);
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
        const choices = currentQuiz.choices;
        const options = choices[0].options;
        const reasons = [];
        for (const option of options) {
          reasons.push(option.reason);
          const node = { node: nodes.length, name: option.label };
          nodes.push(node);
        }
        const uniques = reasons.filter(
          (value, index, array) => array.indexOf(value) === index
        );
        for (const category of uniques) {
          const node = {
            node: nodes.length,
            name: category ? category : "None",
          };
          nodes.push(node);
        }
        for (const entry of result) {
          for (const assignment in entry.assignments) {
            const value = entry.assignments[assignment];
            const source = nodes.find((node) => node.name === assignment).node;
            const target = nodes.find((node) => node.name === entry.label).node;
            const link = { source: source, target: target, value: value };
            links.push(link);
          }
        }
        const json = { nodes: nodes, links: links };
        generator.extent([
          [16, 16],
          [width - 16, height - 16],
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
          .attr("x", (d) => d.x1 + 16)
          .attr("y", function (d) {
            return d.y0 + (d.y1 - d.y0) / 2;
          })
          .attr("dy", ".35em")
          .attr("text-anchor", "end")
          .attr("transform", null)
          .text(function (d) {
            return d.name;
          })
          .filter(function (d) {
            return d.x < width / 2;
          })
          .attr("x", 6 + generator.nodeWidth())
          .attr("text-anchor", "start");

        // the function for moving the nodes
        function dragmove(d) {
          console.log(d);
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
  if (!slide.quizzes[0]) {
    connectionIndicator.hidden = true;
    qrButton.hidden = true;
    stateButton.hidden = true;
    tallySpan.hidden = true;
    return;
  }
  if (!hostClient) {
    try {
      await initializeHost();
    } catch (error) {
      return;
    }
  }
  if (quizState === "ERROR") {
    connectionIndicator.hidden = false;
    qrButton.hidden = true;
    stateButton.hidden = true;
    tallySpan.hidden = true;
    hostClient.tryReconnect();
  }
  if (quizState === "ACTIVE") {
    hostClient.requestEvaluation();
    setQuizState("AWAITING_EVALUATION");
  }
  tallySpan.innerText = "";
  connectionIndicator.hidden = false;
  qrButton.hidden = false;
  stateButton.hidden = false;
  tallySpan.hidden = true;
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
