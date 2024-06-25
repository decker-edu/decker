import Client from "./client.mjs";
import Renderer, { resetAssignmentState } from "./renderer.mjs";
import bwip from "../examiner/bwip.js";
import "../../vendor/d3.v6.min.js";

let Reveal;
let hostClient = undefined;
let currentQuiz = undefined;
let quizState = "WAITING";

let qrButton = document.createElement("button");
let qrDialog = document.createElement("dialog");
let qrCanvas = document.createElement("canvas");
let qrLink = document.createElement("a");

let stateButton = document.createElement("button");
let stateSpan = document.createElement("span");

function setQuizState(state) {
  quizState = state;
  stateSpan.hidden = true;
  delete stateButton.dataset["state"];
  if (state === "WAITING") {
    stateButton.dataset["state"] = "WAITING";
  } else if (state === "ACTIVE") {
    stateSpan.hidden = false;
    stateButton.dataset["state"] = "ACTIVE";
  }
}

function parseQuizzes(reveal) {
  const slides = reveal.getSlides();
  for (const slide of slides) {
    slide.quizzes = [];
    const quizzers = slide.querySelectorAll(":scope .quizzer");
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
      if (question) {
        question.classList.add("question");
        quizObject.question = question.innerHTML;
      }
      const lists = quizzer.querySelectorAll(":scope > ul");
      console.log(":scope > ul:", lists.length);
      for (const list of lists) {
        const choiceObject = {
          votes: 1,
          options: [],
        };
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
        if (
          choiceObject.options.filter((choice) => choice.correct).length > 1
        ) {
          choiceObject.votes = choiceObject.options.length;
        }
        quizObject.choices.push(choiceObject);
      }
      while (quizzer.lastElementChild) {
        quizzer.lastElementChild.remove();
      }
      quizzer.quiz = quizObject;
      if (quizObject.type === "choice") {
        Renderer.renderChoiceQuiz(quizzer, quizObject);
      } else if (quizObject.type === "freetext") {
        Renderer.renderFreeTextQuiz(quizzer, quizObject);
      } else if (quizObject.type === "selection") {
        Renderer.renderSelectionQuiz(quizzer, quizObject);
      } else if (quizObject.type === "assignment") {
        Renderer.renderAssignmentQuiz(quizzer, quizObject);
      }
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
  qrDialog.appendChild(qrCanvas);
  qrDialog.appendChild(qrLink);
  document.body.appendChild(qrDialog);

  qrButton.addEventListener("click", (event) => {
    qrDialog.showModal();
  });
  stateButton.addEventListener("click", async (event) => {
    if (!hostClient) {
      await initializeHost();
    }
    if (quizState === "WAITING") {
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

  stateSpan.className = "quizzer-state-info";

  anchors.placeButton(qrButton, "BOTTOM_CENTER");
  anchors.placeButton(stateButton, "BOTTOM_CENTER");
  anchors.placeButton(stateSpan, "BOTTOM_CENTER");
}

function onStateUpdate(connections, done) {
  stateSpan.innerText = `${done} / ${connections}`;
  if (quizState === "ACTIVE") {
    stateSpan.hidden = false;
  }
}

async function initializeHost() {
  return new Promise((resolve, reject) => {
    hostClient = new Client();
    hostClient.on("state", onStateUpdate);
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
      resolve();
    });
    hostClient.on("result", (result) => {
      quizState = "WAITING";
      stateSpan.hidden = true;
      const resultContainer = document.createElement("div");
      if (currentQuiz && currentQuiz.type === "choice") {
        resultContainer.classList.add("quizzer-choice-results-container");
        const entryContainer = document.createElement("div");
        entryContainer.classList.add("quizzer-choice-result");
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
        resultContainer.classList.add("quizzer-freetext-results-container");
        for (const words of result) {
          const entryContainer = document.createElement("div");
          entryContainer.classList.add("quizzer-freetext-result");
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
        resultContainer.classList.add("quizzer-selection-results-container");
        for (const selection of result) {
          const entryContainer = document.createElement("div");
          entryContainer.classList.add("quizzer-selection-result");
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
          .attr("width", width)
          .attr("height", height)
          .attr("viewBox", [0, 0, width, height]);
        resultContainer.classList.add("quizzer-assignment-results-container");
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
            const link = { source: source, target: target, value: value + 1 };
            links.push(link);
          }
        }
        const testNodes = [
          { node: 0, name: "src0" },
          { node: 1, name: "src1" },
          { node: 2, name: "sink0" },
        ];
        const testLinks = [
          { source: 0, target: 2, value: 4 },
          { source: 1, target: 2, value: 6 },
        ];
        const path = generator.links();
        const json = { nodes: testNodes, links: testLinks };
        //          generator.nodes(testNodes).links(testLinks);
        generator.extent([
          [16, 16],
          [width - 16, height - 16],
        ]);
        var graph = generator(json);
        console.log(graph);
        var link = svg
          .append("g")
          .selectAll(".link")
          .data(testLinks)
          .enter()
          .append("path")
          .attr("class", "link")
          .attr("d", d3.sankeyLinkHorizontal())
          .style("stroke-width", function (d) {
            return Math.max(1, d.dy);
          })
          .sort(function (a, b) {
            return b.dy - a.dy;
          });

        // add in the nodes
        var node = svg
          .append("g")
          .selectAll(".node")
          .data(testNodes)
          .enter()
          .append("g")
          .attr("class", "node")
          .attr("transform", function (d) {
            return "translate(" + d.x0 + "," + d.y0 + ")";
          })
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
          .attr("x", -6)
          .attr("y", function (d) {
            return d.dy / 2;
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

async function onSlideChange(event) {
  resetAssignmentState();
  if (!event.currentSlide) {
    return;
  }
  const slide = event.currentSlide;
  if (!slide.quizzes[0]) {
    qrButton.hidden = true;
    stateButton.hidden = true;
    stateSpan.hidden = true;
    return;
  }
  if (!hostClient) {
    await initializeHost();
  }
  if (quizState === "ACTIVE") {
    hostClient.requestEvaluation();
    setQuizState("AWAITING_EVALUATION");
  }
  stateSpan.innerText = "";
  qrButton.hidden = false;
  stateButton.hidden = false;
  stateSpan.hidden = false;
}

const Plugin = {
  id: "quizzerPlugin",
  init: (reveal) => {
    if (reveal.isSpeakerNotes()) {
      return;
    }
    createHostInterface(reveal);
    parseQuizzes(reveal);
    reveal.on("slidechanged", onSlideChange);
    Reveal = reveal;
  },
};

export default Plugin;
