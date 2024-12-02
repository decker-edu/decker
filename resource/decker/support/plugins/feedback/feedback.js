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
    send_credentials: undefined,
  };

  reveal = undefined;
  config = undefined;

  engine = {
    api: undefined,
    deckId: undefined,
    token: undefined,
  };

  usertoken = undefined;

  open_button = undefined;
  button_badge = undefined;

  position = undefined;

  menu = {
    container: undefined,
    badge: undefined,
    close_button: undefined,
    feedback_list: undefined,
    feedback_input: undefined,
    feedback_send_button: undefined,
    feedback_login_area: undefined,
    feedback_login_button: undefined,
    feedback_credentials: {
      container: undefined,
      username_input: undefined,
      password_input: undefined,
      login_button: undefined,
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
   * Creates the engine object using the base as the URL to send messages to.
   * The deckId is a unique identifier that identifies this
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
   * Connects to the engine endpoint and fetches the usertoken and other credentials.
   * After a token was fetched, the connection is considered established and we can
   * create the interface.
   */
  async prepareEngine() {
    try {
      const token = await this.engine.api.getToken(this.engine.deckId);
      this.engine.token = token;
      const that = this; // Capture this pointer for setupInterface function call
      async function setupInterface() {
        that.createInterface();
        await that.requestMenuContent();
        await that.requestSlideMenuUpdate();
        const previousState = localStorage.getItem("feedback-state");
        if (previousState === "open") {
          that.openMenu();
        }
      }
      if (this.reveal.isReady()) {
        await setupInterface();
      } else {
        this.reveal.addEventListener("ready", setupInterface);
      }
    } catch (error) {
      console.error("API function getToken() failed: ", error);
    }
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
      // This is necessary for the handout plugin because it disables change of the "currentSlide" of Reveal.
      // TODO: Find a better way to deal with this
      if (!document.documentElement.classList.contains("handout"))
        this.requestMenuContent();
      this.reveal.getRevealElement().inert = true;
      // localStorage.setItem("feedback-state", "open");
      this.glass.classList.add("show");
      this.menu.close_button.focus();
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
      this.glass.classList.remove("show", "blur");
      this.open_button.focus();
    }
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
      this.menu.feedback_login_button.classList.remove("fa-sign-out-alt");
      this.menu.feedback_login_button.classList.add("fa-sign-in-alt");
      this.menu.feedback_login_button.setAttribute(
        "title",
        this.localization.interface.login_as_admin
      );
      this.menu.feedback_login_button.setAttribute(
        "aria-label",
        this.localization.interface.login_as_admin
      );
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
  async sendLogin() {
    let credentials = {
      login: this.menu.feedback_credentials.username_input.value,
      password: this.menu.feedback_credentials.password_input.value,
      deck: this.engine.deckId,
    };
    try {
      this.menu.feedback_credentials.password_input.classList.remove("error");
      const token = await this.engine.api.getLogin(credentials);
      this.engine.token.admin = token.admin;
      this.menu.feedback_login_area.classList.add("admin");
      this.menu.feedback_credentials.username_input.value = "";
      this.menu.feedback_credentials.password_input.value = "";
      this.menu.feedback_credentials.container.classList.remove("visible");
      this.menu.feedback_login_button.classList.remove("fa-sign-in-alt");
      this.menu.feedback_login_button.classList.add("fa-sign-out-alt");
      this.menu.feedback_login_button.setAttribute(
        "title",
        this.localization.interface.logout_as_admin
      );
      this.menu.feedback_login_button.setAttribute(
        "aria-label",
        this.localization.interface.logout_as_admin
      );
      this.requestMenuContent();
    } catch (error) {
      console.error(error);
      this.menu.feedback_credentials.password_input.value = "";
      this.menu.feedback_credentials.password_input.classList.add("error");
      this.menu.feedback_credentials.password_input.focus();
    }
  }

  /**
   * Publishes a comment to the engine.
   * @param {*} event
   */
  async sendComment(event) {
    let slideId = this.reveal.getCurrentSlide().id;
    if (
      document.documentElement.classList.contains("handout") &&
      this.mostRecentSlideID
    ) {
      slideId = this.mostRecentSlideID;
    }
    if (this.menu.feedback_input.hasAttribute("answer")) {
      try {
        await this.engine.api.postAnswer(
          this.menu.feedback_input.commentId,
          this.engine.token.admin,
          this.menu.feedback_input.value,
          null
        );
        this.clearTextArea();
        await this.requestMenuContent();
        await this.requestSlideMenuUpdate();
      } catch (error) {
        console.error(error);
      }
    } else {
      try {
        await this.engine.api.submitComment(
          this.engine.deckId,
          slideId,
          this.engine.token.admin || this.usertoken,
          this.menu.feedback_input.value,
          this.menu.feedback_input.commentId,
          window.location.toString()
        );
        this.clearTextArea();
        await this.requestMenuContent();
        await this.requestSlideMenuUpdate();
      } catch (error) {
        console.error(error);
      }
    }
    event.stopPropagation();
    event.preventDefault();
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
   * Sends an async request to the engine to get the questions of the requested slide or
   * the current reveal slide if the slide is not specified.
   * @returns A promise that resolves when the update is finished
   */
  async requestMenuContent(slide) {
    let slideId;
    if (!slide) {
      slideId = this.reveal.getCurrentSlide().id;
    } else {
      slideId = slide.id;
    }
    this.mostRecentSlideID = slideId;
    try {
      const token = this.engine.token.admin || this.usertoken;
      const deckId = this.engine.deckId;
      const list = await this.engine.api.getComments(deckId, slideId, token);
      this.updateMenuContent(list);
    } catch (error) {
      console.error(error);
    }
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
  async deleteQuestion(comment) {
    try {
      const token = this.engine.token.admin || this.usertoken;
      await this.engine.api.deleteComment(comment.id, token);
      await this.requestMenuContent();
      await this.requestSlideMenuUpdate();
    } catch (error) {
      console.error(error);
    }
  }

  /**
   * Deletes all answers to a comment.
   * @param {*} comment
   */
  async resetAnswers(comment) {
    for (let answer of comment.answers) {
      try {
        await this.engine.api.deleteAnswer(answer.id, this.engine.token.admin);
      } catch (error) {
        console.error(error);
      }
    }
    await this.requestMenuContent();
  }

  /**
   * Deletes the passed answer.
   * @param {*} answer
   */
  async deleteAnswer(answer) {
    try {
      await this.engine.api.deleteAnswer(answer.id, this.engine.token.admin);
    } catch (error) {
      console.error(error);
    }
    await this.requestMenuContent();
  }

  /**
   * Marks a question as answered (does not need to have a written answer)
   * @param {*} comment
   */
  async markQuestion(comment) {
    try {
      await this.engine.api.postAnswer(comment.id, this.engine.token.admin);
    } catch (error) {
      console.error(error);
    }
    await this.requestMenuContent();
  }

  /**
   * Gives the comment a +1 or revokes it if this user has already done so.
   * @param {*} comment
   */
  async voteComment(comment) {
    let vote = {
      comment: comment.id,
      voter: this.usertoken,
    };
    try {
      await this.engine.api.voteComment(vote);
    } catch (error) {
      console.error(error);
    }
    await this.requestMenuContent();
  }

  /**
   * Creates a question list item that represents a question.
   * @param {*} comment
   * @returns
   */
  createQuestionContainer(comment) {
    let text = this.localization.question_container;

    let isAdmin = this.engine.token.admin != null;
    let isAuthor = comment.author === this.usertoken;
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
          ? `<button class="fa fa-reply feedback-answer-question-button" title="${text.add}" aria-label="${text.add}"></button>`
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
    MathJax.typeset([this.menu.feedback_list]);
    this.menu.feedback_list.scrollTop = 0;
  }

  /**
   * Initializes the value of the token_input field based on stored values or
   * a random value if none stored was found.
   */
  initializeUsertoken() {
    let localToken = window.localStorage.getItem("feedback-user-token");
    if (this.engine && this.engine.token && this.engine.token.authorized) {
      // If you are logged in externally
      this.usertoken = this.engine.token.authorized;
      this.menu.container.classList.add("authorized");
    } else if (localToken) {
      // If you already have a token in localstorage
      this.usertoken = localToken;
    } else {
      // If you have none of that, take a random token and save it to localstorage
      this.usertoken = this.engine.token.random;
      window.localStorage.setItem("feedback-user-token", this.usertoken);
    }
  }

  /**
   * Requests the api to send a list of questions to update the main slide menu.
   * @returns Promise that resolves when the update is finished
   */
  async requestSlideMenuUpdate() {
    try {
      const list = await this.engine.api.getComments(this.engine.deckId);
      this.updateSlideMenu(list);
    } catch (error) {
      console.error(error);
    }
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
    let button_string = String.raw`<button class="fa-button open-button fas fa-question-circle" title="${text.open_label}" aria-label="${text.open_label}" aria-controls="feedback-menu" aria-haspopup="menu">
      <div class="feedback-badge"></div>
    </button>`;

    let menu_string = String.raw`<div id="feedback-menu" class="feedback-menu slide-in-right" role="menu" inert>
      <div class="feedback-header">
        <div class="counter">0</div>
        <div class="feedback-title">${text.menu_title}</div>
        <button class="fa-button feedback-close fas fa-times-circle" title="${text.menu_close}" aria-label="${text.menu_close}" role="menuitem">
        </button>
      </div>
      <div class="feedback-list"></div>
      <div class="feedback-question-input">
        <textarea wrap="hard" placeholder="${this.localization.question_placeholder}" tabindex="0"></textarea> 
        <button class="feedback-send-button" aria-label="${this.localization.send_comment}"><span class="fas fa-paper-plane"></span><span>${this.localization.send_comment}</span></button>
      </div>
      <div class="feedback-footer">
        <div class="feedback-login">
          <button id="feedback-login-button" class="fa-button fas fa-sign-in-alt" title="${text.login_as_admin}" aria-label="${text.login_as_admin}"></button>
        </div>
        <div class="feedback-credentials">
          <input id="feedback-username" placeholder="${text.username_placeholder}">
          <input id="feedback-password" placeholder="${text.password_placeholder}" type="password">
          <button id="feedback-login-send" type="button" title="${text.send_credentials}" aria-label="${text.send_credentials}">Admin Login</button>
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
    this.menu.feedback_send_button = menu.querySelector(
      ".feedback-send-button"
    );
    this.menu.badge = menu.querySelector(".counter");
    this.menu.feedback_list = menu.querySelector(".feedback-list");
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
    this.menu.feedback_credentials.login_button = menu.querySelector(
      "#feedback-login-send"
    );

    /* Add EventListeners */

    this.open_button.addEventListener("click", () => this.openMenu());

    this.menu.feedback_input.addEventListener("keypress", (e) =>
      e.stopPropagation()
    );
    this.menu.feedback_send_button.addEventListener("click", (e) =>
      this.sendComment()
    );
    this.menu.close_button.addEventListener("click", (event) =>
      this.closeMenu()
    );
    this.menu.feedback_login_button.addEventListener("click", (event) =>
      this.toggleLoginArea()
    );
    this.menu.feedback_input.addEventListener("keydown", (event) => {
      if (event.key === "Enter" && event.shiftKey) {
        this.sendComment(event);
      }
    });

    this.menu.feedback_credentials.password_input.addEventListener(
      "keydown",
      (event) => {
        this.menu.feedback_credentials.password_input.classList.remove("error");
        if (event.key === "Enter") {
          this.sendLogin();
        }
      }
    );

    this.menu.feedback_credentials.login_button.addEventListener(
      "click",
      (event) => {
        this.sendLogin();
      }
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
  }
}

const printMode = /print-pdf/gi.test(window.location.search);

let plugin = () => {
  return {
    id: "feedback",

    getEngine: undefined,
    requestMenuContent: undefined,

    init(reveal) {
      if (printMode) return;
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
        send_comment: "Send<br>Message",
        interface: {
          open_label: "Open Feedback Menu",
          menu_title: "Questions",
          menu_close: "Close Feedback Menu",
          login_as_admin: "Login as Admin",
          logout_as_admin: "Logout as Admin",
          username_placeholder: "Username",
          password_placeholder: "Password",
          send_credentials: "Send credentials",
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
          send_comment: "Nachricht<br>senden",
          interface: {
            open_label: "Fragemenu öffnen",
            menu_title: "Fragen",
            menu_close: "Fragemenu schließen",
            login_as_admin: "Als Administrator einloggen",
            logout_as_admin: "Als Administrator abmelden",
            username_placeholder: "Benutzername",
            password_placeholder: "Passwort",
            send_credentials: "Anmeldedaten absenden",
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
