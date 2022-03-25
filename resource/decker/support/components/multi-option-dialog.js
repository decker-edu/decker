let localization;

import {
  handleKeyboard,
  showFlyingFocus,
} from "../flyingFocus/flying-focus.js";

function createElement({
  type,
  id,
  classes,
  text,
  title,
  parent,
  onclick = null,
}) {
  let element = document.createElement(type);
  if (id) element.id = id;
  if (classes) element.className = classes;
  if (text) element.innerText = text;
  if (title) element.title = title;
  if (title) element.setAttribute("aria-label", title);
  if (parent) parent.appendChild(element);
  if (onclick) element.addEventListener("click", onclick);
  return element;
}

class MultiDialog extends HTMLDialogElement {
  constructor(message, options, severity, callback) {
    super();
    this.message = message ? message : "No Message";
    this.options = options
      ? options
      : [
          { text: localization.accept, value: "OK" },
          { text: localization.deny, value: "CANCEL" },
        ];
    this.severity = severity ? severity : "default";
    switch (this.severity) {
      case "error":
        this.classList.add("error");
        break;
      case "warning":
        this.classList.add("warning");
        break;
      case "alert":
        this.classList.add("alert");
        break;
      case "information":
        this.classList.add("information");
        break;
    }
    this.classList.add("multi-dialog-container");
    this.role = "dialog";
    this.fieldset = createElement({
      type: "fieldset",
      classes: "multi-dialog-fieldset",
      parent: this,
    });
    this.legend = createElement({
      type: "legend",
      id: Math.round(Date.now()).toString(36),
      classes: "multi-dialog-message",
      text: message,
      parent: this.fieldset,
    });
    this.setAttribute("aria-labelledby", this.legend.id);
    this.setAttribute("tabindex", -1);
    this.fieldset.setAttribute("role", "document");
    this.buttonArea = createElement({
      type: "menu",
      classes: "multi-dialog-buttons",
      parent: this.fieldset,
    });
    this.buttons = [];
    for (let option of this.options) {
      let button = createElement({
        type: "button",
        title: option.text,
        classes: "multi-dialog-button",
        parent: this.buttonArea,
      });
      button.innerText = option.text;
      this.buttons.push(button);
      button.addEventListener("click", (event) => callback(option.value));
      button.addEventListener("click", (event) => this.close());
    }
    // Trap Frocus - on Firefox the focus escapes into the window if we do not do this
    this.addEventListener("keydown", (event) => {
      if (event.key === "Escape") {
        event.preventDefault();
        event.stopPropagation();
        this.cancel();
      }
      if (event.key === "Tab") {
        event.preventDefault();
        let len = this.focusable.length - 1;
        let index = this.focusable.indexOf(event.target);
        index = event.shiftKey ? index - 1 : index + 1;
        if (index < 0) index = len;
        if (index > len) index = 0;
        this.focusable[index].focus();
      }
    });
  }

  cancel() {
    this.buttons[this.buttons.length - 1].dispatchEvent(new Event("click"));
  }

  close() {
    super.close();
    this.parentElement.removeChild(this);
  }

  get focusable() {
    return [
      ...this.querySelectorAll(
        'button,[href],select,textarea,input:not([type="hidden"]),[tabindex]:not([tabindex="-1"])'
      ),
    ];
  }
}

window.customElements.define("multi-dialog", MultiDialog, {
  extends: "dialog",
});

window.showDialog = (message, options, severity) => {
  return new Promise((resolve, reject) => {
    let callback = (value) => {
      resolve(value);
    };
    let dialog;
    if (typeof message === "string") {
      dialog = new MultiDialog(message, options, severity, callback);
    } else if (typeof message === "object") {
      let msg = message.message;
      let opt = message.options;
      let sev = message.severity;
      dialog = new MultiDialog(msg, opt, sev, callback);
    }
    document.body.appendChild(dialog);
    dialog.showModal();
    dialog.focus();
  });
};

(function () {
  localization = {
    accept: "OK",
    deny: "Cancel",
  };
  let lang = navigator.language;
  if (lang === "de") {
    localization = {
      accept: "OK",
      deny: "Abbrechen",
    };
  }
})();
