export { prepareEngine };

async function prepareEngine(api) {
  if (Reveal.isReady()) {
    buildInterface(api);
  } else {
    Reveal.addEventListener("ready", event => {
      buildInterface(api);
    });
  }
}

function buildInterface(api) {
  let body = document.querySelector("body");

  let open = document.createElement("div");

  let panel = document.createElement("div");
  let header = document.createElement("div");
  let user = document.createElement("input");
  let check = document.createElement("div");
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

  let lock = document.createElement("i");
  lock.classList.add("far", "fa-lock", "lock");

  let unlock = document.createElement("i");
  unlock.classList.add("far", "fa-unlock", "unlock");

  let qmark = document.createElement("i");
  qmark.classList.add("far", "fa-question-circle");

  panel.classList.add("q-panel");
  open.appendChild(qmark);
  open.classList.add("q-open");

  header.classList.add("q-header");
  user.setAttribute("type", "text");
  user.setAttribute("placeholder", "Enter user token");
  check.setAttribute("title", "Store user token (session)");
  check.classList.add("q-check");
  check.appendChild(lock);
  check.appendChild(unlock);
  header.appendChild(user);
  header.appendChild(check);
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

  document.body.appendChild(open);
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

  let updateUser = () => {
    let val = window.localStorage.getItem("token");
    if (val !== null) {
      user.value = val;
      user.setAttribute("disabled", true);
      check.classList.add("checked");
      user.type = "password";
    } else {
      user.removeAttribute("disabled");
      check.classList.remove("checked");
      user.type = "text";
    }
  };

  let renderDelete = () => {
    api.updateCommentList(getContext, renderList);
  };

  let renderSubmit = () => {
    text.value = "";
    api.updateCommentList(getContext, renderList);
  };

  let renderList = list => {
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

  close.addEventListener("click", _ => {
    open.classList.remove("checked");
    panel.classList.remove("open");
  });

  open.addEventListener("click", _ => {
    open.classList.add("checked");
    panel.classList.add("open");
    api.updateCommentList(getContext, renderList);
    document.activeElement.blur();
  });

  user.addEventListener("keydown", e => {
    if (e.key === "Enter") {
      api.updateCommentList(getContext, renderList);
      e.stopPropagation();
      document.activeElement.blur();
    }
  });

  check.addEventListener("click", _ => {
    if (check.classList.contains("checked") && user.value) {
      check.classList.remove("checked");
      window.localStorage.removeItem("token");
      user.removeAttribute("disabled");
      user.type = "text";
    } else {
      check.classList.add("checked");
      window.localStorage.setItem("token", user.value);
      user.setAttribute("disabled", true);
      user.type = "password";
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
  updateUser();

  Reveal.addEventListener("slidechanged", event => {
    api.updateCommentList(getContext, renderList);
    updateIds();
    updateUser();
  });
}
