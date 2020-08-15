export { prepareEngine };

async function prepareEngine(api) {
  if (Reveal.isReady()) {
    buildInterface(api);
  } else {
    Reveal.addEventListener("ready", event => {
      console.log("Reveal ready at: " + event.currentSlide.id);
      buildInterface(api);
    });
  }
}

function buildInterface(api) {
  let body = document.querySelector("body");

  let panel = document.createElement("div");
  let header = document.createElement("div");
  let user = document.createElement("input");
  let close = document.createElement("div");
  let container = document.createElement("div");
  let input = document.createElement("div");
  let text = document.createElement("textarea");
  let footer = document.createElement("div");
  let deckid = document.createElement("input");
  let slideid = document.createElement("input");

  let trash = document.createElement("i");
  trash.classList.add("far", "fa-trash-alt");

  let cross = document.createElement("i");
  cross.classList.add("far", "fa-window-close");

  panel.classList.add("q-panel");
  header.classList.add("q-header");
  user.setAttribute("placeholder", "Enter user token");
  header.appendChild(user);
  header.appendChild(close);
  close.classList.add("q-close");
  close.appendChild(cross);

  container.classList.add("q-list");

  input.classList.add("q-input");
  input.appendChild(text);
  text.setAttribute("rows", 4);
  text.setAttribute("placeholder", "Enter question");

  footer.classList.add("q-footer");
  footer.appendChild(deckid);
  deckid.setAttribute("placeholder", "Deck ID");
  deckid.setAttribute("disabled", true);
  footer.appendChild(slideid);
  slideid.setAttribute("placeholder", "Slide ID");
  slideid.setAttribute("disabled", true);

  panel.appendChild(header);
  panel.appendChild(container);
  panel.appendChild(input);
  panel.appendChild(footer);

  document.body.appendChild(panel);

  let getContext = () => {
    return {
      deck: body.getAttribute("data-deckid"),
      slide: Reveal.getCurrentSlide().id,
      token: user.value
    };
  };

  let updateIds = () => {
    let context = getContext();
    deckid.value = context.deck;
    slideid.value = context.slide;
  };

  let renderDelete = () => {
    api.updateCommentList(getContext, renderList);
  };

  let renderSubmit = () => {
    text.value = "";
    api.updateCommentList(getContext, renderList);
  };

  let renderList = list => {
    console.log(list);
    while (container.firstChild) {
      container.removeChild(container.lastChild);
    }
    for (let comment of list) {
      let content = document.createElement("div");
      content.classList.add("content");
      content.textContent = comment.html;

      let div = document.createElement("div");
      div.appendChild(content);
      

      if (comment.delete) {
        let del = document.createElement("button");
        del.appendChild(trash.cloneNode(true));
        del.addEventListener("click", _ => {
          api.deleteComment(getContext, comment.delete, renderDelete);
        });
        div.appendChild(del);
      }
      container.appendChild(div);
    }
    container.scrollTop = 0;
  };

  user.addEventListener("keydown", e => {
    if (e.key === "Enter") {
      api.updateCommentList(getContext, renderList);
      updateIds();
      e.stopPropagation();
      document.activeElement.blur();
    }
  });

  text.addEventListener("keydown", e => {
    if (e.key === "Enter" && e.shiftKey) {
      api.submitComment(getContext, text.value, renderSubmit);
      e.stopPropagation();
      e.preventDefault();
      document.activeElement.blur();
    }
  });

  api.updateCommentList(getContext, renderList);
  updateIds();

  Reveal.addEventListener("slidechanged", event => {
    console.log(getContext());
    console.log(event.currentSlide.id);
    api.updateCommentList(getContext, renderList);
    updateIds();
  });
}
