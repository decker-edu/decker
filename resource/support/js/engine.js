export { prepareEngine };

// TODO Make into a proper Reveal plugin
async function prepareEngine(api) {
  if (Reveal.isReady()) {
    buildInterface(api);
  } else {
    Reveal.addEventListener("ready", _ => {
      buildInterface(api);
    });
  }
}

async function buildInterface(api) {
  var serverToken;
  api
    .getToken()
    .then(t => (serverToken = t))
    .catch(e => {
      // Nothing goes without a token
      console.log("getToken() failed: " + e);
      console.log("retrying ...");
      setTimeout(buildInterface, 1000);
    });

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

  let edit = document.createElement("i");
  edit.classList.add("far", "fa-edit");

  let cross = document.createElement("i");
  cross.classList.add("far", "fa-times-circle");

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
  text.setAttribute("wrap", "hard");
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

  // Found on StackOverflow
  const hashCode = s =>
    s.split("").reduce((a, b) => ((a << 5) - a + b.charCodeAt(0)) | 0, 0);

  let url = new URL(window.location.href);
  let refererId = hashCode(url.host + url.pathname);

  let getContext = () => {
    return {
      deck: refererId,
      slide: Reveal.getCurrentSlide().id,
      token: user.value
    };
  };

  let updateIds = () => {
    let context = getContext();
    deckid.value = context.deck;
    slideid.value = context.slide;
  };

  let initUser = () => {
    let localToken = window.localStorage.getItem("token");
    if (serverToken && serverToken.authorized) {
      user.value = serverToken.authorized;
      user.setAttribute("disabled", true);
      check.classList.add("checked");
      user.type = "password";
    } else if (localToken) {
      user.value = localToken;
      user.setAttribute("disabled", true);
      check.classList.add("checked");
      user.type = "password";
    } else {
      user.value = hashCode(Math.random().toString());
      user.removeAttribute("disabled");
      check.classList.remove("checked");
      user.type = "text";
    }
  };

  let updateComments = () => {
    let context = getContext();
    api
      .getComments(context.deck, context.slide, context.token)
      .then(renderList)
      .catch(console.log);
  };

  let renderSubmit = () => {
    updateComments();
    text.value = "";
  };

  let renderList = list => {
    while (container.firstChild) {
      container.removeChild(container.lastChild);
    }
    for (let comment of list) {
      let content = document.createElement("div");
      content.classList.add("content");
      content.innerHTML = comment.html;

      let item = document.createElement("div");
      item.classList.add("item");
      item.appendChild(content);

      if (comment.delete) {
        let box = document.createElement("div");
        let del = document.createElement("button");
        del.appendChild(trash.cloneNode(true));
        del.addEventListener("click", _ => {
          let context = getContext();
          api.deleteComment(comment.delete, context.token).then(updateComments);
        });
        let mod = document.createElement("button");
        mod.appendChild(edit.cloneNode(true));
        mod.addEventListener("click", _ => {
          let context = getContext();
          api.deleteComment(comment.delete, context.token).then(updateComments);
          text.value = comment.markdown;
        });
        box.appendChild(mod);
        box.appendChild(del);
        item.appendChild(box);
      }
      container.appendChild(item);
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
    updateComments();
    document.activeElement.blur();
  });

  if (!(serverToken && serverToken.authorized)) {
    user.addEventListener("keydown", e => {
      if (e.key === "Enter") {
        updateComments();
        e.stopPropagation();
        document.activeElement.blur();
      }
    });

    check.addEventListener("click", _ => {
      if (check.classList.contains("checked")) {
        check.classList.remove("checked");
        window.localStorage.removeItem("token");
        user.removeAttribute("disabled");
        user.type = "text";
      } else {
        if (user.value) {
          check.classList.add("checked");
          window.localStorage.setItem("token", user.value);
          user.setAttribute("disabled", true);
          user.type = "password";
        }
      }
      updateComments();
    });
  }

  text.addEventListener("keydown", e => {
    if (e.key === "Enter" && e.shiftKey) {
      let context = getContext();
      api
        .submitComment(context.deck, context.slide, context.token, text.value)
        .then(renderSubmit)
        .catch(console.log);
      e.stopPropagation();
      e.preventDefault();
      document.activeElement.blur();
    }
  });

  Reveal.addEventListener("slidechanged", _ => {
    updateComments();
    updateIds();
    initUser();
  });

  updateComments();
  updateIds();
  initUser();
}
