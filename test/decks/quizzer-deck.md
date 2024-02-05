---
title: Quizzer Test Deck
poll-server: wss://archytas.cs.tu-dortmund.de/quizzer/quiz
---

# Quizslide

<style>
:root {
/* quizzes */
  --green-background-color: var(--accent3-bbg);
  --green-border-color: var(--accent3);
  --red-background-color: var(--accent0-bbg);
  --red-border-color: var(--accent0);
}

#quiz-connection-indicator {
  cursor: help;
}

#quiz-connection-indicator.ok {
  color: var(--greenish);
}

#quiz-connection-indicator.error {
  color: var(--redish);
}

/* Poll from tudo */

/* quiz flex layout */
.reveal .quiz ul {
  display: flex;
  flex-direction: row;
  flex-wrap: wrap;
  justify-content: center;
  align-items: center;
  align-content: center;
  text-align: center;
  counter-reset: answer;
  margin-top: 1em;
}

/* possibility to hide quizzes */
.hide-quizzes .reveal .quiz,
.hide-quizzes .reveal .quiz * {
  visibility: hidden;
}

/* one quiz item */
.reveal .quiz ul li {
  display: inline-block;
  position: relative;
  text-align: left;
  margin: 0.5em;
  border: 3px solid var(--blueish);
  border-radius: 0.5em;
  padding: 0.5em;
  font-weight: normal;
  background-color: var(--shade1);
  cursor: pointer;
}

/* add A, B, C, D... */
.reveal .quiz ul.task-list > li::before {
  content: counter(answer, upper-latin) ": ";
  counter-increment: answer;
  margin-right: 0.5em;
  font-weight: bold;
}

/* remove task list checkbox */
.reveal .quiz ul.task-list li > input[type="checkbox"] {
  display: none;
}

/* 2nd level ul contains tool tip */
.reveal .quiz ul.task-list li ul {
  display: inline-block;
  margin: 0;
  padding: 0;
}

/* don't need this for 2nd level */
.reveal .quiz ul.task-list li ul li::before {
  display: none;
}

/* tool tip */
.reveal .quiz ul li ul li {
  display: block;
  visibility: hidden;
  max-width: 95%;
  top: 100%;
  left: 5px;
  background-color: grey;
  color: #ffffff;
  text-align: center;
  margin: 5px;
  padding: 5px;
  border-radius: 6px;
  border: none;
  position: absolute;
  z-index: 1;
  font-size: 1rem;
}
.reveal .show-answer:hover ul li {
  visibility: visible;
}

/* once clicked, wrong answers are red */
.reveal .quiz ul li.wrong {
  --quiz-background-color: var(--red-background-color);
  --quiz-border-color: var(--red-border-color);
  --quiz-icon: "\f00d";
}

/* once clicked, correct answers are green */
.reveal .quiz ul li.right {
  --quiz-background-color: var(--green-background-color);
  --quiz-border-color: var(--green-border-color);
  --quiz-icon: "\f00c";
}

.reveal .quiz ul li.show-answer {
  background-color: var(--quiz-background-color);
  border-color: var(--quiz-border-color);
}
.reveal .quiz ul li.show-answer::after {
  font-style: normal;
  font-variant: normal;
  text-rendering: auto;
  font-weight: 900;
  font-family: "Font Awesome 5 Free";
  content: var(--quiz-icon);
  position: absolute;
  font-size: 15px;
  right: 0.2em;
  bottom: -0.1em;
  padding: 0.1em;
  color: var(--quiz-border-color);
}

/*********************************************
 * override quiz-wue settings
 *********************************************/

.reveal [class*="quiz-"] button {
  font: inherit;
  font-size: 0.9em;
  margin: 0.5em;
  padding: 0.5em;
  color: var(--shade6);
  background: var(--shade1);
  border: 2px solid var(--icon-active-color);
  border-radius: 0.5em;
}

.reveal .quiz-ft button {
  display: none;
}

.reveal .quiz-ft input {
  font: inherit;
  font-size: 0.8em;
  margin: 0.5em;
  padding: 0.5em;
  border-radius: 0.25em;
  border: 2px solid var(--blueish);
  color: var(--shade6);
  background-color: var(--shade1);
  outline: none;
}

.reveal .qft-solutions,
.reveal .qft-solutions.solved {
  display: none;
}

.reveal .quiz-ic select {
  font: inherit;
  font-size: 0.9em;
  font-weight: normal;
  border: 2px solid var(--blueish);
  color: var(--shade6);
  background-color: var(--shade1);
  outline: none;
  margin: 0.5em;
  padding: 0.5em;
  border-radius: 0.25em;
}

.reveal .quiz-ic option {
  font: inherit;
  font-size: inherit;
  font-weight: normal;
}

.reveal .quiz-mi {
  width: calc(
    var(--slide-width) - 2 * var(--block-outset) - 2 * var(--block-border-width)
  );
  text-align: center;
}

.reveal .quiz-mi .buckets,
.reveal .quiz-mi .matchItems {
  box-sizing: border-box;
  border: 2px solid var(--shade3);
  padding: 15px 10px;
  gap: 10px;
}

.reveal .quiz-mi .buckets {
  margin-bottom: 20px;
  margin-top: 60px;
}

.reveal .quiz-mi .buckets:before,
.reveal .quiz-mi .matchItems:before {
  position: absolute;
  top: -20px;
  left: 10px;
  font-size: 15px;
  background-color: var(--shade0);
  padding: 5px;
  color: var(--shade3);
}

.reveal .quiz-mi .matchItems:after {
  position: absolute;
  bottom: -30px;
  border-left: 30px solid transparent;
  border-right: 30px solid transparent;
  border-top: 30px solid var(--shade3);
}

.reveal .quiz-mi .bucket {
  display: block;
  flex: 1 0;
  margin: 0;
  padding: 10px;
  border: 1px solid var(--shade3);
}

.reveal .quiz-mi .matchItem {
  display: block;
  overflow: hidden;
  width: fit-content;
  border: 2px solid var(--blueish);
  border-radius: 10px;
  padding: 10px;
  cursor: grab;
  margin: auto;
  background-color: var(--shade1);
}

.reveal [class*="quiz"] .show-right {
  background-color: var(--green-background-color);
  border-color: var(--green-border-color);
  --quiz-icon: "\f00c";
  --quiz-border-color: var(--green-border-color);
  position: relative;
}

.reveal [class*="quiz"] .show-wrong {
  background-color: var(--red-background-color);
  border-color: var(--red-border-color);
  --quiz-icon: "\f00d";
  --quiz-border-color: var(--red-border-color);
  position: relative;
}

.reveal [class*="quiz-"] .show-right::after,
.reveal [class*="quiz-"] .show-wrong::after {
  font-style: normal;
  font-variant: normal;
  text-rendering: auto;
  font-weight: 900;
  font-family: "Font Awesome 5 Free";
  content: var(--quiz-icon);
  position: absolute;
  font-size: 15px;
  right: 0.2em;
  bottom: -0.1em;
  padding: 0.1em;
  color: var(--quiz-border-color);
}

/*********************************************
 * quiz polling
 *********************************************/

.reveal-viewport:not(.poll) .poll-only {
  display: none;
}

#poll-chart {
  font-size: var(--icon-size);
  position: absolute;
  left: auto;
  top: auto;
  right: 0.5em;
  bottom: 0.5em;
  visibility: hidden;
  z-index: 34;
  width: 420px;
  height: 320px;
  margin: auto;
  padding: 0.5em;
  text-align: center;
  border: 3px solid var(--blueish);
  border-radius: 0.5em;
  background-color: var(--shade0);
  transform-origin: bottom right;
  box-shadow: 3px 5px 5px var(--shade3);
}

#poll-chart.canvas {
  top: 0;
  left: 0;
  width: 400px;
  height: 300px;
}

#poll-votes {
  font-size: var(--icon-size);
  color: var(--icon-inactive-color);
  text-align: center;
  display: none;
}

#qrcode-container {
  display: none;
  z-index: 4000;
  position: absolute;
  left: 0;
  right: 0;
  bottom: 0;
  top: 0;
  height: 100%;
  background: rgba(0, 0, 0, 0.5);
  backdrop-filter: blur(6px);
  flex-flow: column nowrap;
  align-items: center;
  justify-content: center;
  gap: 0.5em;
}
#qrcode-container.show {
  display: flex;
}

#qrcode-canvas {
  height: 66vh;
  min-height: 0;
  aspect-ratio: 1 / 1;
  background-color: white;
  padding: 0.5em;
  border-radius: 0.25em;
}
#qrcode-canvas.smaller {
  height: 33vh;
}

#qrcode-link {
  display: block;
  font-size: 2rem;
  font-weight: bold;
  padding: 0.5em;
  border-radius: 0.25em;
  background-color: var(--background-color);
}

#close-qr-button {
  position: absolute;
  right: 0;
  top: 0;
  z-index: 4001;
  background-color: var(--background-color);
  border-radius: 100%;
  color: var(--icon-inactive-color);
}
#close-qr-button:hover {
  color: var(--icon-active-color);
}

</style>

## Multiple Choice Quiz

::: quiz

- [X] Correct
  - Because Yes
- [ ] Wrong
  - Because No

:::

# Intermission

# Another Quizslide

## Another Quiz

::: quiz

- [ ] Another Wrong Answer
  - Because No
- [X] Another Correct Answer
  - Because Yes
- [ ] Yet Another Wrong Answer
  - Because No

:::

# No Right Answer

## It is all wrong!

::: quiz

- [ ] Wrong
  - Wrong Wrong Wrong
- [ ] Wrong Wrong
  - Wrong
- [ ] Wrong
  - Click

:::