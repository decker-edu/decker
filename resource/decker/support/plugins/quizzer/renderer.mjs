export default class Renderer {
  static renderChoiceQuiz(parent, quiz) {
    const container = document.createElement("div");
    container.classList.add("choice-quiz");
    const questionParagraph = document.createElement("p");
    questionParagraph.classList.add("question");
    questionParagraph.innerHTML = quiz.question;
    container.appendChild(questionParagraph);
    if (quiz.choices.length > 1) {
      const error = document.createElement("p");
      error.innerText =
        "Malformed Quiz Data: Multiple answer sets for a multiple choice quiz.";
      return;
    }
    const answers = quiz.choices[0];
    console.log(answers);
    for (const answer of answers.options) {
      const button = document.createElement("button");
      button.classList.add("answer");
      button.addEventListener("click", (event) => {
        if (answer.correct) {
          button.classList.add("correct");
        } else {
          button.classList.add("wrong");
        }
      });
      button.innerHTML = answer.answer;
      container.appendChild(button);
    }
    parent.appendChild(container);
  }
}
