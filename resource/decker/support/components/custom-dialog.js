/* default: en */
let localization = {
  submit: "Submit",
  deny: "Cancel",
};

class CustomDialog extends HTMLDialogElement {
  constructor({ title, content, options, severity, callback }) {
    super();
    options = options
      ? options
      : [
          { text: localization.submit, value: "SUBMIT" },
          { text: localization.deny, value: "CANCEL" },
        ];
    if (options.length < 1) {
      throw "Options need to have at least one entry.";
    }
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

    /* I am creating the elements manually here as the fixed HTML I can use here only *
     * consists of two lines of code, so using the template way seems fruitless here  */
    this.form = document.createElement("form");
    this.form.method = "dialog";
    this.appendChild(this.form);

    this.form.fieldset = document.createElement("fieldset");
    this.form.fieldset.className = "multi-dialog-fieldset";
    this.form.appendChild(this.form.fieldset);

    this.form.fieldset.legend = document.createElement("legend");
    this.form.fieldset.legend.id = Math.round(Date.now()).toString(36);
    this.form.fieldset.legend.className = "multi-dialog-message";
    this.form.fieldset.legend.innerText = title;
    this.form.fieldset.setAttribute("role", "document");
    this.form.fieldset.appendChild(this.form.fieldset.legend);

    this.setAttribute("aria-labelledby", this.form.fieldset.legend.id);
    this.setAttribute("tabindex", -1);

    if (content) {
      this.form.fieldset.appendChild(content);
    }

    this.form.fieldset.buttonArea = document.createElement("menu");
    this.form.fieldset.buttonArea.className = "multi-dialog-buttons";
    this.form.fieldset.appendChild(this.form.fieldset.buttonArea);
    this.form.fieldset.buttons = [];
    for (let option of options) {
      let button = document.createElement("button");
      button.type = "button";
      button.title = option.text;
      button.className = "multi-dialog-button";
      button.innerText = option.text;
      button.value = option.value;
      this.form.fieldset.buttonArea.appendChild(button);
      this.form.fieldset.buttons.push(button);
      button.addEventListener("click", (event) =>
        this.submit(button.value, callback)
      );
      button.addEventListener("click", (event) => this.close());
    }
    // Trap Focus - on Firefox the focus escapes into the window if we do not do this
    this.addEventListener("keydown", (event) => {
      if (event.key === "Enter") {
        event.preventDefault();
        event.stopPropagation();
        this.confirm();
      }
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

  submit(submitValue, callback) {
    let object = this.collectFormData(new FormData(this.form));
    object.submit = submitValue;
    callback(object);
  }

  collectFormData(formData) {
    const result = {};
    formData.forEach((value, key) => {
      if (!(key in result)) {
        result[key] = value;
        return;
      }
      if (!Array.isArray(result[key])) {
        result[key] = [result[key]];
      }
      result[key].push(value);
    });
    return result;
  }

  confirm() {
    this.form.fieldset.buttons[0].dispatchEvent(new Event("click"));
  }

  cancel() {
    let last = this.form.fieldset.buttons.length - 1;
    this.form.fieldset.buttons[last].dispatchEvent(new Event("click"));
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

window.customElements.define("multi-dialog-two", CustomDialog, {
  extends: "dialog",
});

window.showDialog = (message, content, options, severity) => {
  return new Promise((resolve, reject) => {
    let callback = (value) => {
      resolve(value);
    };
    let dialog = new CustomDialog({
      title: message,
      content: content,
      options: options,
      severity: severity,
      callback: callback,
    });

    document.body.appendChild(dialog);
    dialog.showModal();
    dialog.focus();
  });
};

window.showInformation = (message, content) => {
  return new Promise((resolve, reject) => {
    let callback = (value) => {
      resolve(value);
    };
    let dialog = new CustomDialog({
      title: message,
      content: content,
      options: [{ text: "OK", value: "confirmed" }],
      severity: "information",
      callback: callback,
    });

    document.body.appendChild(dialog);
    dialog.showModal();
    dialog.focus();
  });
};

window.showChoice = (message, options, severity) => {
  severity = severity ? severity : "information";
  return new Promise((resolve, reject) => {
    let callback = (value) => {
      resolve(value);
    };
    let dialog = new CustomDialog({
      title: message,
      content: null,
      options: options,
      severity: severity,
      callback: callback,
    });

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
