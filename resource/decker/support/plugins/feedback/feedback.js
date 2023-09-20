/**
 * Plugin that adds a menu for viewers of the slides to give feedback and ask
 * questions.
 *
 * @author Henrik Tramberend
 * @author Sebastian Hauer (rewrite)
 */
import client from "./api-client.js";

class Feedback {
  timeout = 500;

  lang = "en";
  localization = {
    question_placeholder: "",
    answer_placeholder: "",
    interface: undefined,
    question_container: undefined,
    answer_container: undefined,
  };

  reveal = undefined;
  config = undefined;

  engine = {
    api: undefined,
    deckId: undefined,
    token: undefined,
  };

  open_button = undefined;
  button_badge = undefined;

  position = undefined;

  menu = {
    container: undefined,
    badge: undefined,
    token_input: undefined,
    token_lock: undefined,
    token_icon: undefined,
    lock_label: undefined,
    close_button: undefined,
    feedback_list: undefined,
    feedback_input: undefined,
    feedback_login_area: undefined,
    feedback_login_button: undefined,
    feedback_credentials: {
      container: undefined,
      username_input: undefined,
      password_input: undefined,
    },
  };

  glass = undefined;

  constructor(position) {
    this.position = position;
  }

  /**
   * Returns only the very basic url of the current window.
   */
  get deckURL() {
    let url = new URL(window.location);
    url.hash = "";
    url.query = "";
    url.username = "";
    url.password = "";
    return url.toString();
  }

  /**
   * Tries to establish a connection to the engine by downloading the implementation
   * from the given base. The deckId is a unique identifier that identifies this
   * deck and can be set in either the deck.yaml or the markdown of the deck.
   * @param {*} base A URL.
   * @param {*} deckId A unique id from deck.yaml or the deck markdown.
   */
  createEngine(base, deckId) {
    this.engine.deckId = deckId || this.deckURL;
    this.engine.api = new client(base);
    this.prepareEngine();
  }

  /**
   * Sets up the engine fetched previously by contactEngine.
   */
  prepareEngine() {
    this.engine.api
      .getToken(this.engine.deckId)
      .then((token) => {
        // Globally set the server token.
        this.engine.token = token;

        // Build the panel, once Reval is ready.
        if (this.reveal.isReady()) {
          this.createInterface();
        } else {
          this.reveal.addEventListener("ready", (_) => {
            this.createInterface();
          });
        }
      })
      .catch((e) => {
        // Nothing goes without a token
        console.log("API function getToken() failed: " + e);
        throw e;
      });
  }

  /**
   * Turns the menu on or off, based on its current state.
   */
  toggleMenu() {
    if (this.menu.container.inert) this.openMenu();
    else this.closeMenu();
  }

  /**
   * Opens the menu and updates its content. Also focuses the first button in the menu.
   */
  openMenu() {
    if (this.menu.container.inert) {
      this.menu.container.inert = false;
      this.menu.token_lock.focus();
      // This is necessary for the handout plugin because it disables change of the "currentSlide" of Reveal.
      // TODO: Find a better way to deal with this
      if (!document.documentElement.classList.contains("handout"))
        this.requestMenuContent();
      this.reveal.getRevealElement().inert = true;
      // localStorage.setItem("feedback-state", "open");
      this.glass.classList.add("show");
      this.menu.token_lock.focus();
    }
  }

  /**
   * Closes the menu and focuses the button that opened it.
   */
  closeMenu() {
    if (!this.menu.container.inert) {
      this.menu.container.inert = true;
      this.reveal.getRevealElement().inert = false;
      localStorage.removeItem("feedback-state");
      this.glass.classList.remove("show");
      this.open_button.focus();
    }
  }

  /**
   * Disables or enables the token_input field.
   */
  toggleTokenInput() {
    if (this.menu.token_lock.getAttribute("aria-checked") === "true") {
      this.unlockTokenInput();
    } else {
      if (this.menu.token_input.value) {
        //disallow empty token
        this.lockTokenInput();
      }
    }
  }

  /**
   * Disables the token input field and stores its value in the local storage.
   */
  lockTokenInput() {
    this.menu.token_input.setAttribute("disabled", true);
    this.menu.token_input.type = "password";
    this.menu.token_lock.setAttribute(
      "title",
      this.localization.interface.unlock_token
    );
    this.menu.token_lock.setAttribute(
      "aria-label",
      this.localization.interface.unlock_token
    );
    this.menu.token_lock.setAttribute("aria-checked", "true");
    this.menu.token_icon.classList.remove("fa-unlock");
    this.menu.token_icon.classList.add("fa-lock");
    window.localStorage.setItem(
      "feedback-user-token",
      this.menu.token_input.value
    );
  }

  /**
   * Enables input on the token input field and deletes the token from the local
   * storage until it is locked again.
   */
  unlockTokenInput() {
    this.menu.token_input.removeAttribute("disabled");
    this.menu.token_input.type = "text";
    this.menu.token_input.classList.remove("hidden");
    this.menu.token_lock.setAttribute(
      "title",
      this.localization.interface.lock_token
    );
    this.menu.token_lock.setAttribute(
      "aria-label",
      this.localization.interface.lock_token
    );
    this.menu.token_lock.setAttribute("aria-checked", "false");
    this.menu.token_icon.classList.remove("fa-lock");
    this.menu.token_icon.classList.add("fa-unlock");
    /* Remove this because currently we no longer use a text label on the lock button.
     * this.menu.lock_label.textContent = this.localization.interface.lock_token;
     */
    window.localStorage.removeItem("feedback-user-token");
  }

  /**
   * Completely hides the token input. Called if you are the admin.
   */
  hideTokenInput() {
    this.lockTokenInput();
    this.menu.token_input.classList.add("hidden");
    this.menu.token_lock.classList.add("hidden");
  }

  /**
   * Turns on or off the login credentials area in the footer.
   */
  toggleLoginArea() {
    if (this.menu.feedback_login_area.classList.contains("admin")) {
      this.engine.token.admin = null;
      this.menu.feedback_credentials.username_input.value = "";
      this.menu.feedback_credentials.password_input.value = "";
      this.menu.feedback_login_area.classList.remove("admin");
      this.menu.feedback_credentials.container.classList.remove("visible");
      this.requestMenuContent();
    } else {
      if (
        this.menu.feedback_credentials.container.classList.contains("visible")
      ) {
        this.menu.feedback_credentials.container.classList.remove("visible");
      } else {
        this.menu.feedback_credentials.container.classList.add("visible");
        this.menu.feedback_credentials.username_input.focus();
      }
    }
  }

  /**
   * Tries to perfom a login with the entered credentials.
   */
  sendLogin(event) {
    if (event.key === "Enter") {
      let credentials = {
        login: this.menu.feedback_credentials.username_input.value,
        password: this.menu.feedback_credentials.password_input.value,
        deck: this.engine.deckId,
      };
      this.engine.api
        .getLogin(credentials)
        .then((token) => {
          this.engine.token.admin = token.admin;
          this.menu.feedback_login_area.classList.add("admin");
          this.menu.feedback_credentials.username_input.value = "";
          this.menu.feedback_credentials.password_input.value = "";
          this.menu.feedback_credentials.container.classList.remove("visible");
          this.requestMenuContent();
        })
        .catch((error) => {
          console.error(error);
          this.menu.feedback_credentials.password_input.value = "";
        });
    }
  }

  /**
   * Publishes a comment to the engine.
   * @param {*} event
   */
  sendComment(event) {
    if (event.key === "Enter" && event.shiftKey) {
      let slideId = this.reveal.getCurrentSlide().id;
      if (
        document.documentElement.classList.contains("handout") &&
        this.mostRecentSlideID
      ) {
        slideId = this.mostRecentSlideID;
      }
      if (this.menu.feedback_input.hasAttribute("answer")) {
        this.engine.api
          .postAnswer(
            this.menu.feedback_input.commentId,
            this.engine.token.admin,
            this.menu.feedback_input.value,
            null
          )
          .then(() => this.clearTextArea())
          .then(() => this.requestMenuContent())
          .then(() => this.requestSlideMenuUpdate())
          .catch(console.log);
      } else {
        this.engine.api
          .submitComment(
            this.engine.deckId,
            slideId,
            this.engine.token.admin || this.menu.token_input.value,
            this.menu.feedback_input.value,
            this.menu.feedback_input.commentId,
            window.location.toString()
          )
          .then(() => this.clearTextArea())
          .then(() => this.requestMenuContent())
          .then(() => this.requestSlideMenuUpdate())
          .catch(console.log);
      }
      event.stopPropagation();
      event.preventDefault();
    }
  }

  /**
   * Clears the text area and resets its state.
   */
  clearTextArea() {
    this.menu.feedback_input.value = "";
    this.menu.feedback_input.placeholder =
      this.localization.question_placeholder;
    this.menu.feedback_input.commentId = null;
    this.menu.feedback_input.removeAttribute("answer");
  }

  /**
   * Updates the badges of the open-button and the menu header.
   * @param {*} value
   * @param {*} answered
   */
  updateBadges(value, answered) {
    this.button_badge.textContent = value;
    this.button_badge.setAttribute("data-count", value);
    this.menu.badge.textContent = value;
    this.menu.badge.setAttribute("data-count", value);
    if (answered) {
      this.button_badge.classList.add("answered");
      this.menu.badge.classList.add("answered");
    } else {
      this.button_badge.classList.remove("answered");
      this.menu.badge.classList.remove("answered");
    }
  }

  /**
   * Simply removes all visible questions (from the UI, not the engine)
   */
  clearQuestionList() {
    while (this.menu.feedback_list.firstChild) {
      this.menu.feedback_list.removeChild(this.menu.feedback_list.lastChild);
    }
  }

  /**
   * Sends an async request to the engine to get the questions of the current slide.
   * @returns A promise that resolves when the update is finished
   */
  requestMenuContent(slide) {
    let slideId;
    if (!slide) {
      slideId = this.reveal.getCurrentSlide().id;
    } else {
      slideId = slide.id;
    }
    this.mostRecentSlideID = slideId;
    return this.engine.api
      .getComments(
        this.engine.deckId,
        slideId,
        this.engine.token.admin || this.menu.token_input.value
      )
      .then((list) => this.updateMenuContent(list))
      .catch(console.log);
  }

  /**
   * Prepares the text area to answer a question.
   * @param {*} comment
   */
  answerQuestion(comment) {
    this.menu.feedback_input.value = "";
    this.menu.feedback_input.commentId = comment.id;
    this.menu.feedback_input.setAttribute("answer", true);
    this.menu.feedback_input.placeholder = this.localization.answer_placeholder;
    this.menu.feedback_input.focus();
  }

  /**
   * Prepares the text area to edit a question.
   * @param {*} comment
   */
  editQuestion(comment) {
    this.menu.feedback_input.value = comment.markdown;
    this.menu.feedback_input.commentId = comment.id;
    this.menu.feedback_input.removeAttribute("answer");
    this.menu.feedback_input.placeholder =
      this.localization.question_placeholder;
    this.menu.feedback_input.focus();
  }

  /**
   * Deletes the passed comment.
   * @param {*} comment
   */
  deleteQuestion(comment) {
    this.engine.api
      .deleteComment(
        comment.id,
        this.engine.token.admin || this.menu.token_input.value
      )
      .then(() => this.requestMenuContent())
      .then(() => this.requestSlideMenuUpdate());
  }

  /**
   * Deletes all answers to a comment.
   * @param {*} comment
   */
  resetAnswers(comment) {
    let chain = Promise.resolve();
    for (let answer of comment.answers) {
      chain.then(() =>
        this.engine.api.deleteAnswer(answer.id, this.engine.token.admin)
      );
    }
    chain.then(() => this.requestMenuContent());
  }

  /**
   * Deletes the passed answer.
   * @param {*} answer
   */
  deleteAnswer(answer) {
    this.engine.api
      .deleteAnswer(answer.id, this.engine.token.admin)
      .then(() => this.requestMenuContent());
  }

  /**
   * Marks a question as answered (does not need to have a written answer)
   * @param {*} comment
   */
  markQuestion(comment) {
    this.engine.api
      .postAnswer(comment.id, this.engine.token.admin)
      .then(() => this.requestMenuContent());
  }

  /**
   * Gives the comment a +1 or revokes it if this user has already done so.
   * @param {*} comment
   */
  voteComment(comment) {
    let vote = {
      comment: comment.id,
      voter: this.menu.token_input.value,
    };
    this.engine.api.voteComment(vote).then(() => this.requestMenuContent());
  }

  /**
   * Creates a question list item that represents a question.
   * @param {*} comment
   * @returns
   */
  createQuestionContainer(comment) {
    let text = this.localization.question_container;

    let isAdmin = this.engine.token.admin != null;
    let isAuthor = comment.author === this.menu.token_input.value;
    let isDeletable = isAdmin || (isAuthor && comment.answers.length == 0);
    let isAnswered = comment.answers && comment.answers.length > 0;

    let template = document.createElement("template");
    template.innerHTML = String.raw`<div class="feedback-item">
  <div class="feedback-content">
    ${comment.html}
  </div>
  <div class="feedback-controls">
    <div class="feedback-controls-wrapper">
      <span class="votes" title="${text.votes}" aria-label="${text.votes}">${
      comment.votes > 0 ? comment.votes : ""
    }</span>
      <button class="${comment.didvote ? "fas" : "far"} fa-thumbs-up vote ${
      !isAuthor ? "canvote" : "cantvote"
    } ${comment.didvote ? "didvote" : ""}"
        title="${comment.didvote ? text.downvote : text.upvote}"
        aria-label="${comment.didvote ? text.downvote : text.upvote}">
      </button>
      ${
        isDeletable
          ? `<button class="fas fa-edit feedback-edit-question-button" title="${text.edit}" aria-label="${text.edit}"></button>`
          : ""
      }
      ${
        isDeletable
          ? `<button class="fas fa-trash-alt feedback-delete-question-button" title="${text.delete}" aria-label="${text.delete}"></button>`
          : ""
      }
      ${
        isAdmin
          ? `<button class="far fa-plus-square feedback-answer-question-button" title="${text.add}" aria-label="${text.add}">`
          : ""
      }
      ${
        isAnswered
          ? `<button class="far fa-check-circle answered feedback-reset-answers-button" title="${
              isDeletable ? text.reset : text.answered
            }" aria-label="${isDeletable ? text.reset : text.answered}" ${
              !isDeletable ? "disabled" : ""
            }></button>`
          : `<button class="far fa-circle notanswered feedback-mark-answered-button" title="${
              isDeletable ? text.mark : text.notanswered
            }" aria-label="${isDeletable ? text.mark : text.notanswered}" ${
              !isDeletable ? "disabled" : ""
            }></button>`
      }
    </div>
  </div>
</div>`;
    let question = template.content.firstElementChild;
    if (!isAuthor) {
      let voteButton = question.querySelector(".vote");
      voteButton.addEventListener("click", (event) =>
        this.voteComment(comment)
      );
    }
    if (isDeletable) {
      let editButton = question.querySelector(".feedback-edit-question-button");
      editButton.addEventListener("click", (event) =>
        this.editQuestion(comment)
      );
      let deleteButton = question.querySelector(
        ".feedback-delete-question-button"
      );
      deleteButton.addEventListener("click", (event) =>
        this.deleteQuestion(comment)
      );
      if (isAnswered && isAdmin) {
        let resetButton = question.querySelector(
          ".feedback-reset-answers-button"
        );
        resetButton.addEventListener("click", (event) =>
          this.resetAnswers(comment)
        );
      }
      if (!isAnswered && isAdmin) {
        let answerButton = question.querySelector(
          ".feedback-mark-answered-button"
        );
        answerButton.addEventListener("click", (event) =>
          this.markQuestion(comment)
        );
      }
    }
    if (isAdmin) {
      let addButton = question.querySelector(
        ".feedback-answer-question-button"
      );
      addButton.addEventListener("click", (event) =>
        this.answerQuestion(comment)
      );
    }
    MathJax.typeset([question]);
    return question;
  }

  /**
   * Creates a question list item that represents an answer.
   * @param {*} answer
   * @returns
   */
  createAnswerContainer(answer) {
    let isAdmin = this.engine.token.admin != null;
    let text = this.localization.answer_container;
    let url = answer.link ? new URL(answer.link) : undefined;
    let html = answer.html ? answer.html : "";
    let template = document.createElement("template");
    template.innerHTML = String.raw`
      <div class="feedback-item answer">
        <div class="feedback-content">
          ${html}
        </div>
        <div class="feedback-controls">
          ${
            isAdmin
              ? `
            <button class="fas fa-trash-alt feedback-delete-answer-button" title="${text.delete}" aria-label="${text.delete}">
            </button>`
              : ""
          }
          ${
            url
              ? `
            <a href="${url}" target="_blank">
              <i class="fas fa-external-link-alt"></i>
            </a>`
              : ""
          }
        </div>
      </div>`;
    let item = template.content.cloneNode(true);
    if (isAdmin) {
      let deleteButton = item.querySelector(".feedback-delete-answer-button");
      deleteButton.addEventListener("click", () => this.deleteAnswer(answer));
    }
    MathJax.typeset([item]);
    return item;
  }

  /**
   * Updates the question menu content with the question list.
   * @param {*} list
   */
  updateMenuContent(list) {
    let allAnswered = true;
    for (let comment of list) {
      const isAnswered = comment.answers && comment.answers.length > 0;
      if (!isAnswered) {
        allAnswered = false;
        break;
      }
    }

    this.updateBadges(list.length, allAnswered);

    this.clearQuestionList();

    // re-fill question container
    for (let comment of list) {
      let item = this.createQuestionContainer(comment);
      this.menu.feedback_list.appendChild(item);
      for (let answer of comment.answers) {
        let block = this.createAnswerContainer(answer);
        this.menu.feedback_list.appendChild(block);
      }
    }

    this.menu.feedback_list.scrollTop = 0;
  }

  /**
   * Initializes the value of the token_input field based on stored values or
   * a random value if none stored was found.
   */
  initializeUsertoken() {
    let localToken = window.localStorage.getItem("feedback-user-token");
    if (this.engine && this.engine.token && this.engine.token.authorized) {
      this.menu.token_input.value = this.engine.token.authorized;
      this.menu.container.classList.add("authorized");
      this.hideTokenInput();
    } else if (localToken) {
      this.menu.token_input.value = localToken;
      this.lockTokenInput();
    } else {
      this.menu.token_input.value = this.engine.token.random;
      this.lockTokenInput();
    }
  }

  /**
   * Requests the api to send a list of questions to update the main slide menu.
   * @returns Promise that resolves when the update is finished
   */
  requestSlideMenuUpdate() {
    return this.engine.api
      .getComments(this.engine.deckId)
      .then((list) => this.updateSlideMenu(list))
      .catch(console.log);
  }

  /**
   * Counts the questions for each slide and adds a badge in the slide menu.
   * Now talks to the slide menu via its plugin instead of using querySelectors.
   * @param {*} list
   */
  updateSlideMenu(list) {
    let menu_plugin = this.reveal.getPlugin("decker-menu");
    let slide_list = menu_plugin.getSlideList();
    for (let item of slide_list) {
      item.removeAttribute("data-questions");
      item.removeAttribute("data-answered");
    }
    for (let comment of list) {
      const slideID = comment.slide;
      const slide = document.getElementById(slideID);
      if (slide) {
        let item = menu_plugin.getListItemByID(slideID);
        if (item) {
          let questions = item.hasAttribute("data-questions")
            ? parseInt(item.getAttribute("data-questions"))
            : 0;
          let answered = item.hasAttribute("data-answered")
            ? item.getAttribute("data-answered") === "true"
            : true;

          questions = questions + 1;
          answered = answered && comment.answers.length > 0;

          item.setAttribute("data-questions", questions);
          item.setAttribute("data-answered", answered);
        }
      } else {
        //Major error: Slide not found
        console.warn("Could not find slide with ID: " + slideID);
      }
    }
  }

  /**
   * Sets up the whole interface: Button and the right hidden question menu.
   */
  createInterface() {
    let text = this.localization.interface;
    let button_string = String.raw`<button class="fa-button open-button fas fa-question-circle" title="${text.open_label}" aria-label="${text.open_label}">
      <div class="feedback-badge"></div>
    </button>`;

    let menu_string = String.raw`<div class="feedback-menu  slide-in-right" inert>
      <div class="feedback-header">
        <div class="counter">0</div>
        <div class="feedback-title">${text.menu_title}</div>
        <input class="feedback-token-input" type="password" placeholder="${text.token_placeholder}" disabled="true"></input>
        <button class="fa-button feedback-lock fas fa-lock lock-icon" role="switch" aria-checked="true" title="${text.unlock_token}" aria-label="${text.unlock_token}">
        </button>
        <button class="fa-button feedback-close fas fa-times-circle" title="${text.menu_close}" aria-label="${text.menu_close}">
        </button>
      </div>
      <div class="feedback-list"></div>
      <div class="feedback-question-input">
        <textarea wrap="hard" placeholder="${this.localization.question_placeholder}" tabindex="0"></textarea> 
      </div>
      <div class="feedback-footer">
        <div class="feedback-login">
          <button id="feedback-login-button" class="fa-button fas fa-sign-in-alt" title="${text.login_as_admin}" aria-label="${text.login_as_admin}"></button>
        </div>
        <div class="feedback-credentials">
          <input id="feedback-username" placeholder="${text.username_placeholder}">
          <input id="feedback-password" placeholder="${text.password_placeholder}" type="password">
        </div>
      </div>
    </div>`;

    let button_template = document.createElement("template");
    let menu_template = document.createElement("template");
    button_template.innerHTML = button_string;
    menu_template.innerHTML = menu_string;
    let button = button_template.content.firstChild;
    let menu = menu_template.content.firstChild;

    /* Setup references */

    this.open_button = button;
    this.menu.container = menu;

    this.button_badge = button.querySelector(".feedback-badge");

    this.menu.feedback_input = menu.querySelector(
      ".feedback-question-input textarea"
    );
    this.menu.badge = menu.querySelector(".counter");
    this.menu.feedback_list = menu.querySelector(".feedback-list");
    this.menu.token_input = menu.querySelector(".feedback-token-input");
    this.menu.token_lock = menu.querySelector(".feedback-lock");
    this.menu.token_icon = menu.querySelector(".lock-icon");
    this.menu.close_button = menu.querySelector(".feedback-close");
    this.menu.feedback_login_area = menu.querySelector(".feedback-login");
    this.menu.feedback_login_button = menu.querySelector(
      "#feedback-login-button"
    );
    this.menu.feedback_credentials.container = menu.querySelector(
      ".feedback-credentials"
    );
    this.menu.feedback_credentials.username_input =
      menu.querySelector("#feedback-username");
    this.menu.feedback_credentials.password_input =
      menu.querySelector("#feedback-password");

    /* Add EventListeners */

    this.open_button.addEventListener("click", () => this.openMenu());

    this.menu.feedback_input.addEventListener("keypress", (e) =>
      e.stopPropagation()
    );
    this.menu.close_button.addEventListener("click", (event) =>
      this.closeMenu()
    );
    this.menu.feedback_login_button.addEventListener("click", (event) =>
      this.toggleLoginArea()
    );
    this.menu.token_lock.addEventListener("click", (event) =>
      this.toggleTokenInput()
    );
    this.menu.feedback_input.addEventListener("keydown", (event) =>
      this.sendComment(event)
    );

    this.menu.feedback_credentials.password_input.addEventListener(
      "keydown",
      (event) => this.sendLogin(event)
    );

    this.reveal.addEventListener("slidechanged", () =>
      this.requestMenuContent()
    );
    this.reveal.addEventListener("slidechanged", () =>
      this.requestSlideMenuUpdate()
    );

    /* Place Button in UI */

    if (this.reveal.hasPlugin("ui-anchors")) {
      let anchors = this.reveal.getPlugin("ui-anchors");
      anchors.placeButton(this.open_button, this.position);
    }
    document.body.appendChild(this.menu.container);

    /* Temporary Solution */
    this.glass = document.querySelector("#glass");
    if (!this.glass) {
      this.glass = document.createElement("div");
      this.glass.id = "glass";
      document.body.appendChild(this.glass);
    }
    this.glass.addEventListener("click", (event) => this.closeMenu(event));

    /* Finish setup before presentation */

    this.initializeUsertoken();

    this.requestMenuContent()
      .then(() => this.requestSlideMenuUpdate())
      .then(() => {
        /* Open menu again if it was previously opened */
        const previous_state = localStorage.getItem("feedback-state");
        if (previous_state === "open") {
          this.openMenu();
        }
      });
  }
}

let plugin = () => {
  return {
    id: "feedback",

    getEngine: undefined,
    requestMenuContent: undefined,

    init(reveal) {
      const instance = new Feedback("TOP_RIGHT");
      instance.reveal = reveal;
      instance.config = reveal.getConfig().feedback;
      instance.lang = navigator.language;

      //TODO Get this from external sources

      instance.localization = {
        question_placeholder:
          "Type question, ⇧⏎ (Shift-Return) to enter. Use Markdown for formatting.",
        answer_placeholder:
          "Type answer, ⇧⏎ (Shift-Return) to enter. Use Markdown for formatting.",
        interface: {
          open_label: "Open Feedback Menu",
          menu_title: "Questions",
          token_placeholder: "Usertoken",
          unlock_token: "Unlock Token",
          lock_token: "Lock Token",
          menu_close: "Close Feedback Menu",
          login_as_admin: "Login as Admin",
          username_placeholder: "Username",
          password_placeholder: "Password",
        },
        question_container: {
          upvote: "Up-vote question",
          downvote: "Down-vote question",
          edit: "Edit question",
          delete: "Delete question",
          add: "Add answer",
          mark: "Mark as answered",
          reset: "Mark as not answered",
          answered: "Question has been answered",
          notanswered: "Question has not been answered",
          votes: "Up-Votes",
        },
        answer_container: {
          delete: "Delete answer",
        },
      };

      if (instance.lang === "de") {
        instance.localization = {
          question_placeholder:
            "Frage hier eingeben und mit ⇧⏎ (Umschalt-Eingabe) absenden. Markdown kann zur Formatierung genutzt werden.",
          answer_placeholder:
            "Antwort hier eingeben und mit ⇧⏎ (Umschalt-Eingabe) absenden. Markdown kann zur Formatierung genutzt werden.",
          interface: {
            open_label: "Fragemenu öffnen",
            menu_title: "Fragen",
            token_placeholder: "Nutzertoken",
            lock_token: "Token entsprren",
            unlock_token: "Token sperren",
            menu_close: "Fragemenu schließen",
            login_as_admin: "Als Administrator einloggen",
            username_placeholder: "Benutzername",
            password_placeholder: "Passwort",
          },
          question_container: {
            upvote: "Frage unterstützen",
            downvote: "Unterstützung zurücknehmen",
            edit: "Frage bearbeiten",
            delete: "Frage löschen",
            add: "Antwort hinzufügen",
            mark: "Als beantwortet markieren",
            reset: "Als unbeantwortet markieren",
            answered: "Frage wurde beantwortet",
            notanswered: "Frage wurde noch nicht beantwortet",
            votes: "Stimmen",
          },
          answer_container: {
            delete: "Antwort löschen",
          },
        };
      }

      this.getEngine = () => instance.engine;
      this.requestMenuContent = (slide) => instance.requestMenuContent(slide);

      let url = instance.config?.server || instance.config?.["base-url"];
      let id = instance.config?.deckID || instance.config?.["deck-id"];
      if (url) instance.createEngine(url, id);
    },
  };
};

export default plugin;
