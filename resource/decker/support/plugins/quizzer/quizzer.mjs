import Renderer from "./renderer.mjs";

let Reveal;

function parseQuizzes(reveal) {
  const slides = reveal.getSlides();
  for (const slide of slides) {
    const quizzers = slide.querySelectorAll(":scope .quizzer");
    for (const quizzer of quizzers) {
      const quizObject = {
        type: undefined,
        question: undefined,
        choices: [],
      };
      if (quizzer.classList.contains("dnd")) {
        quizObject.type = "drag&drop";
      } else if (quizzer.classList.contains("freetext")) {
        quizObject.type = "freetext";
      } else if (quizzer.classList.contains("select")) {
        quizObject.type = "select";
      } else {
        quizObject.type = "choice";
      }
      const question = quizzer.querySelector(":scope p");
      question.classList.add("question");
      const lists = quizzer.querySelectorAll(":scope > ul");
      for (const list of lists) {
        const choiceObject = {
          options: [],
        };
        const items = list.querySelectorAll(":scope > li");
        for (const item of items) {
          const answerObject = {
            answer: undefined,
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
          const answer = item.innerHTML;
          answerObject.answer = answer;
          answerObject.correct = item.classList.contains("task-yes");
          choiceObject.options.push(answerObject);
        }
        quizObject.choices.push(choiceObject);
      }
      quizObject.question = question.innerHTML;
      while (quizzer.lastElementChild) {
        quizzer.lastElementChild.remove();
      }
      Renderer.renderChoiceQuiz(quizzer, quizObject);
    }
  }
}

const Plugin = {
  id: "quizzerPlugin",
  init: (reveal) => {
    if (reveal.isSpeakerNotes()) {
      return;
    }
    parseQuizzes(reveal);
    Reveal = reveal;
  },
};

export default Plugin;
