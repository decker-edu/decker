export { contactEngine };

// TODO Make into a proper Reveal plugin

const DEBUG = false;
const DEBUG_AUTH = false;

var timeout = 100; // ms

function contactEngine(base) {
  import(base + "/decker-util.js")
    .then(engine => {
      prepareEngine(engine.buildApi(base));
    })
    .catch(e => {
      console.log("Can't contact decker engine:" + e);
      setTimeout(() => contactEngine(base), (timeout *= 1.5));
    });
}

async function prepareEngine(api) {
  var serverToken;
  api
    .getToken()
    .then(token => {
      serverToken = token;
      if (Reveal.isReady()) {
        buildInterface(api, serverToken);
      } else {
        Reveal.addEventListener("ready", _ => {
          buildInterface(api, serverToken);
        });
      }
    })
    .catch(e => {
      // Nothing goes without a token
      console.log("getToken() failed: " + e);
      console.log("retrying ...");
      setTimeout(() => prepareEngine(api), 1000);
    });
}

async function buildInterface(api, initialToken) {
  var serverToken = initialToken;

  if (DEBUG) {
    console.log("token:", initialToken);
  }

  let open = document.createElement("div");
  let badge = document.createElement("div");

  let panel = document.createElement("div");
  let header = document.createElement("div");
  let title = document.createElement("div");
  let counter = document.createElement("div");
  let user = document.createElement("input");
  let check = document.createElement("div");
  let close = document.createElement("div");
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

  let cross = document.createElement("i");
  cross.classList.add("fas", "fa-times-circle");
  cross.setAttribute("title", "Close panel");

  let lock = document.createElement("i");
  lock.classList.add("fas", "fa-lock", "lock");
  lock.setAttribute("title", "Lock user token");

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
  open.classList.add("q-open");
  open.setAttribute("title", "Open questions panel");
  badge.classList.add("q-badge");

  header.classList.add("q-header");
  title.textContent = "Questions";
  title.classList.add("q-title");
  counter.textContent = "0";
  counter.classList.add("q-counter");
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
  text.addEventListener('keypress', (e) => { e.stopPropagation(); });

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

  // Found on StackOverflow
  const hashCode = s =>
    s.split("").reduce((a, b) => ((a << 5) - a + b.charCodeAt(0)) | 0, 0);

  let url = new URL(window.location);
  url.hash = "";
  url.query = "";
  url.username = "";
  url.password = "";

  let getContext = () => {
    return {
      deck: url,
      slide: Reveal.getCurrentSlide().id,
      token: user.value
    };
  };

  let updateIds = () => {
    let context = getContext();
  };

  let initUser = () => {
    let localToken = window.localStorage.getItem("token");
    if (serverToken && serverToken.authorized) {
      // Some higher power has authorized this user. Lock token in.
      user.value = serverToken.authorized;
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
      user.value = serverToken.random;
      user.removeAttribute("disabled");
      check.classList.remove("checked");
      user.type = "text";
    }
  };

  let updateComments = () => {
    let context = getContext();
    api
      .getComments(
        context.deck,
        context.slide,
        serverToken.admin || context.token
      )
      .then(renderList)
      .catch(console.log);
  };

  let renderSubmit = () => {
    updateComments();
    text.value = "";
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

      if (comment.delete) {
        let box = document.createElement("div");
        let del = document.createElement("button");
        del.appendChild(trash.cloneNode(true));
        del.addEventListener("click", _ => {
          let context = getContext();
          api
            .deleteComment(comment.delete, serverToken.admin || context.token)
            .then(updateComments);
        });
        let mod = document.createElement("button");
        mod.appendChild(edit.cloneNode(true));
        mod.addEventListener("click", _ => {
          let context = getContext();
          api
            .deleteComment(comment.delete, serverToken.admin || context.token)
            .then(updateComments);
          text.value = comment.markdown;
          text.focus();
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

  login.addEventListener("click", _ => {
    if (login.classList.contains("admin")) {
      serverToken.admin = null;
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
      serverToken.admin = null;
      username.value = "";
      password.value = "";
      login.classList.remove("admin");
      credentials.classList.remove("visible");
      updateComments();
    } else {
      api
        .getLogin({ login: username.value, password: password.value })
        .then(token => {
          serverToken.admin = token.admin;
          login.classList.add("admin");
          username.value = "";
          password.value = "";
          credentials.classList.remove("visible");
          updateComments();
        })
        .catch(e => {
          password.value = "";
        });
    }
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
  });

  initUser();
  updateComments();
  updateIds();
}
