export {}

let view = restore();

function record(id, open) {
  if (open) {
    let s = new Set(view.open);
    s.add(id);
    view.open = [...s];
  } else {
    let s = new Set(view.open);
    s.delete(id);
    view.open = [...s];
  }
  view.scrollY = window.scrollY;
}

function restore() {
  let view = localStorage.getItem("view");
  if (view) {
    try {
      view = JSON.parse(view);
      for (let id of view.open) {
        let element = document.getElementById(id);
        let content = element.querySelector("div")
        content.classList.remove("closed");
      }
      window.scrollTo({
        top: view.scrollY,
        behavior: "auto"
      });
      return view;
    } catch (e) {console.log(e);}
  }
  return {
    scrollY: 0,
    open: []
  };
}

function store(_) {
  localStorage.setItem("view", JSON.stringify(view));
}

function init() {
  console.log("[] catalog initializing")

  window.addEventListener("unload", store);

  let lectures = document.querySelectorAll("div.lecture");
  for (let lecture of lectures) {
    let button = lecture.querySelector("h1 > button");
    let content = lecture.querySelector("div")
    button.addEventListener("click", _ => {
      content.classList.toggle("closed");
      record(lecture.id, !content.classList.contains("closed"));
    });

    let topics = lecture.querySelectorAll("div.topic");
    for (let topic of topics) {
      let button = topic.querySelector("h2 > button");
      let content = topic.querySelector("div")
      button.addEventListener("click", _ => {
        content.classList.toggle("closed");
        record(topic.id, !content.classList.contains("closed"));
      });

      let questions = topic.querySelectorAll("div.question");
      for (let question of questions) {
        let button = question.querySelector("h3 > button");
        let content = question.querySelector("div")
        button.addEventListener("click", _ => {
          content.classList.toggle("closed");
          record(question.id, !content.classList.contains("closed"));
        });
      }
    }
  }

}

window.addEventListener("load", init);
