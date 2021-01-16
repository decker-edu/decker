export {}

function init() {
  console.log("[] catalog initializing")

  let lectures = document.querySelectorAll("div.lecture");
  for (let lecture of lectures) {
    let button = lecture.querySelector("h1 > button");
    let content = lecture.querySelector("div")
    button.addEventListener("click", e => {
      content.classList.toggle("closed");
    });

    let topics = lecture.querySelectorAll("div.topic");
    for (let topic of topics) {
      let button = topic.querySelector("h2 > button");
      let content = topic.querySelector("div")
      button.addEventListener("click", e => {
        content.classList.toggle("closed");
      });

      let questions = topic.querySelectorAll("div.question");
      for (let question of questions) {
        let button = question.querySelector("h3 > button");
        let content = question.querySelector("div")
        button.addEventListener("click", e => {
          content.classList.toggle("closed");
        });

      }
    }

  }
}

window.addEventListener("load", init);
