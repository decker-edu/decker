export { contactEngine };

// TODO Make into a proper Reveal 4 plugin

// Start with a 0.5 s retry interval. Back off exponentally.
var timeout = 500;

let engine = {
  api: undefined,
  deckId: undefined, // The unique deck identifier.
  token: undefined
};

// Contacts the engine API at base.
function contactEngine(base, deckId) {
  engine.deckId = deckId || deckUrl();

  // Try to import the API utility module.
  import(base + "/decker-util.js")
    .then(util => {
      console.log("Decker engine contacted at: ", base);
      engine.api = util.buildApi(base);
      prepareEngine();
    })
    .catch(e => {
      console.log("Can't contact decker engine:" + e);
      console.log("Retrying ..." + e);
      setTimeout(() => contactEngine(base, deckId), (timeout *= 2));
    });
}

// Strips the document URI from everything that can not be part of the deck id.
function deckUrl() {
  let url = new URL(window.location);
  url.hash = "";
  url.query = "";
  url.username = "";
  url.password = "";
  return url.toString();
}

// Prepares the questions panel for operation.
function prepareEngine() {
  engine.api
    .getToken()
    .then(token => {
      // Globally set the server token.
      engine.token = token;

      // Build the panel, once Reval is ready.
      if (Reveal.isReady()) {
        buildInterface();
      } else {
        Reveal.addEventListener("ready", _ => {
          buildInterface();
        });
      }

      // Build the menu, once Reval and the menu are ready.
      if (
        Reveal.isReady() &&
        Reveal.hasPlugin("menu") &&
        Reveal.getPlugin("menu").isInit()
      ) {
        buildMenu();
      } else {
        Reveal.addEventListener("menu-ready", _ => {
          buildMenu();
        });
      }
    })
    .catch(e => {
      // Nothing goes without a token
      console.log("API function getToken() failed: " + e);
      throw e;
    });
}

// Builds the panel and sets up event handlers.
function buildInterface() {
  let open = document.createElement("div");
  let badge = document.createElement("div");

  let panel = document.createElement("div");
  let header = document.createElement("div");
  let title = document.createElement("div");
  let counter = document.createElement("div");
  let user = document.createElement("input");
  let check = document.createElement("button");
  let close = document.createElement("button");
  let container = document.createElement("div");
  let input = document.createElement("div");
  let text = document.createElement("textarea");
  let footer = document.createElement("div");
  let login = document.createElement("div");
  let credentials = document.createElement("div");
  let username = document.createElement("input");
  let password = document.createElement("input");

  let trash = document.createElement("i");
  trash.classList.add("fas", "fa-trash-alt");
  trash.setAttribute("title", "Delete question");

  let edit = document.createElement("i");
  edit.classList.add("fas", "fa-edit");
  edit.setAttribute("title", "Edit question");

  let thumb = document.createElement("i");
  thumb.classList.add("far", "fa-thumbs-up");
  thumb.setAttribute("title", "Up-vote question");

  let thumbS = document.createElement("i");
  thumbS.classList.add("fas", "fa-thumbs-up");
  thumbS.setAttribute("title", "Down-vote question");

  let cross = document.createElement("i");
  cross.classList.add("fas", "fa-times-circle");
  cross.setAttribute("title", "Close panel");

  let lock = document.createElement("i");
  lock.classList.add("fas", "fa-lock", "lock");
  lock.setAttribute("title", "Lock user ");

  let unlock = document.createElement("i");
  unlock.classList.add("fas", "fa-unlock", "unlock");
  unlock.setAttribute("title", "User token is locked");

  let gear = document.createElement("i");
  gear.classList.add("fas", "fa-cog", "gears");
  gear.setAttribute("title", "Login as admin");

  let signin = document.createElement("i");
  signin.classList.add("fas", "fa-sign-in-alt", "gears");
  signin.setAttribute("title", "Login as admin");

  let signout = document.createElement("i");
  signout.classList.add("fas", "fa-sign-out-alt", "gears");
  signout.setAttribute("title", "Logout admin");

  let qmark = document.createElement("i");
  qmark.classList.add("fas", "fa-question-circle");

  panel.classList.add("q-panel");
  open.appendChild(qmark);
  open.appendChild(badge);
  open.classList.add("open-button");
  open.setAttribute("title", "Open questions panel");
  badge.classList.add("open-badge", "badge");

  header.classList.add("q-header");
  title.textContent = "Questions";
  title.classList.add("q-title");
  counter.textContent = "0";
  counter.classList.add("counter", "badge");
  user.setAttribute("type", "text");
  user.setAttribute("placeholder", "Enter user token");
  check.setAttribute("title", "Store user token (session)");
  check.classList.add("q-check");
  check.appendChild(lock);
  check.appendChild(unlock);
  header.appendChild(counter);
  header.appendChild(title);
  header.appendChild(user);
  header.appendChild(check);
  header.appendChild(close);
  close.classList.add("q-close");
  close.appendChild(cross);

  container.classList.add("q-list");

  input.classList.add("q-input");
  input.appendChild(text);
  text.setAttribute("wrap", "hard");
  text.setAttribute(
    "placeholder",
    "Type question, ⇧⏎ (Shift - Return) to enter"
  );
  // prevent propagating keypress up to Reveal, since otherwise '?'
  // triggers the help dialog.
  text.addEventListener("keypress", e => {
    e.stopPropagation();
  });

  footer.classList.add("q-footer");
  username.setAttribute("placeholder", "Login");
  password.setAttribute("placeholder", "Password");
  password.type = "password";

  login.appendChild(signin);
  login.classList.add("q-login");

  footer.appendChild(login);
  footer.appendChild(credentials);
  credentials.appendChild(username);
  credentials.appendChild(password);
  credentials.classList.add("credentials");

  panel.appendChild(header);
  panel.appendChild(container);
  panel.appendChild(input);
  panel.appendChild(footer);

  document.body.appendChild(open);
  document.body.appendChild(panel);

  let initUser = () => {
    let localToken = window.localStorage.getItem("token");
    if (engine.token && engine.token.authorized) {
      // Some higher power has authorized this user. Lock token in.
      user.value = engine.token.authorized;
      user.setAttribute("disabled", true);
      check.classList.add("checked");
      user.type = "password";
      check.classList.add("hidden");
      user.classList.add("hidden");
      panel.classList.add("authorized");
      login.classList.add("admin");
    } else if (localToken) {
      user.value = localToken;
      user.setAttribute("disabled", true);
      check.classList.add("checked");
      user.type = "password";
    } else {
      user.value = engine.token.random;
      user.removeAttribute("disabled");
      check.classList.remove("checked");
      user.type = "text";
    }
  };

  let updateComments = () => {
    let slideId = Reveal.getCurrentSlide().id;
    engine.api
      .getComments(engine.deckId, slideId, engine.token.admin || user.value)
      .then(renderList)
      .catch(console.log);
  };

  let renderSubmit = () => {
    updateComments();
    text.value = "";
    text.commentId = null;
    text.answered = null;
  };

  let canDelete = comment => {
    return engine.token.admin !== null || comment.author === user.value;
  };

  let renderList = list => {
    counter.textContent = list.length;
    counter.setAttribute("data-count", list.length);
    badge.textContent = list.length;
    badge.setAttribute("data-count", list.length);

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

      let box = document.createElement("div");
      box.classList.add("controls");
      content.insertBefore(box, content.firstChild);

      // Upvote button
      let vote = document.createElement("button");
      if (comment.didvote) {
        vote.appendChild(thumbS.cloneNode(true));
      } else {
        vote.appendChild(thumb.cloneNode(true));
      }
      vote.classList.add("vote");
      if (comment.author !== user.value) {
        vote.classList.add("canvote");
        if (comment.didvote) {
          vote.classList.add("didvote");
        }
        vote.addEventListener("click", _ => {
          let vote = {
            comment: comment.id,
            voter: user.value
          };
          engine.api.voteComment(vote).then(updateComments);
        });
      } else {
        vote.classList.add("cantvote");
      }
      // Number of upvotes
      let votes = document.createElement("span");

      votes.textContent = comment.votes > 0 ? comment.votes : "";
      votes.classList.add("votes");

      box.appendChild(votes);
      box.appendChild(vote);

      if (canDelete(comment)) {
        // Delete button
        let del = document.createElement("button");
        del.appendChild(trash.cloneNode(true));
        del.addEventListener("click", _ => {
          engine.api
            .deleteComment(comment.id, engine.token.admin || user.value)
            .then(updateComments);
        });
        // Edit button
        let mod = document.createElement("button");
        mod.appendChild(edit.cloneNode(true));
        mod.addEventListener("click", _ => {
          text.value = comment.markdown;
          text.commentId = comment.id;
          text.answered = comment.answered;
          text.focus();
        });
        box.appendChild(mod);
        box.appendChild(del);
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

  login.addEventListener("click", _ => {
    if (login.classList.contains("admin")) {
      engine.token.admin = null;
      username.value = "";
      password.value = "";
      login.classList.remove("admin");
      credentials.classList.remove("visible");
      updateComments();
    } else {
      if (credentials.classList.contains("visible")) {
        credentials.classList.remove("visible");
      } else {
        credentials.classList.add("visible");
      }
    }
  });

  password.addEventListener("keydown", e => {
    if (e.key !== "Enter") return;

    if (login.classList.contains("admin")) {
      engine.token.admin = null;
      username.value = "";
      password.value = "";
      login.classList.remove("admin");
      credentials.classList.remove("visible");
      updateComments();
    } else {
      engine.api
        .getLogin({
          login: username.value,
          password: password.value,
          deck: engine.deckId
        })
        .then(token => {
          engine.token.admin = token.admin;
          login.classList.add("admin");
          username.value = "";
          password.value = "";
          credentials.classList.remove("visible");
          updateComments();
        })
        .catch(_ => {
          password.value = "";
        });
    }
  });

  if (!engine.token.authorized) {
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
      let slideId = Reveal.getCurrentSlide().id;
      engine.api
        .submitComment(
          engine.deckId,
          slideId,
          user.value,
          text.value,
          text.commentId,
          text.answered
        )
        .then(renderSubmit)
        .catch(console.log);
      e.stopPropagation();
      e.preventDefault();
      document.activeElement.blur();
    }
  });

  Reveal.addEventListener("slidechanged", _ => {
    updateComments();
  });

  initUser();
  updateComments();
}

function buildMenu() {
  let updateMenu = list => {
    for (let comment of list) {
      // get slide info
      const slideID = comment.slide;
      const slide = document.getElementById(slideID);
      const indices = Reveal.getIndices(slide);

      // build query string, get menu item
      let query = "ul.slide-menu-items > li.slide-menu-item";
      if (indices.h) query += '[data-slide-h="' + indices.h + '"]';
      if (indices.v) query += '[data-slide-v="' + indices.v + '"]';
      let li = document.querySelector(query);

      // update question counter
      if (li) {
        li.setAttribute(
          "data-questions",
          li.hasAttribute("data-questions")
            ? parseInt(li.getAttribute("data-questions")) + 1
            : 1
        );
      }
    }
  };

  engine.api
    .getComments(engine.deckId)
    .then(updateMenu)
    .catch(console.log);
}
