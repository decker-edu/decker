class AwesomeButton extends HTMLButtonElement {
  constructor() {
    super();
    if (this.hasAttribute("icon-style")) {
      this._iconStyle = this.getAttribute("icon-style");
    } else {
      this._iconStyle = "fas";
    }
    if (this.hasAttribute("icon")) {
      this._icon = this.getAttribute("icon");
    } else {
      this._icon = "fa-square-question";
    }
    this.classList.add(this._iconStyle);
    this.classList.add(this._icon);
  }

  static get observedAttributes() {
    return ["icon", "icon-style"];
  }

  set icon(value) {
    this.setAttribute("icon", value);
  }

  get icon() {
    return this._icon;
  }

  set iconStyle(value) {
    this.setAttribute("icon-style", value);
  }

  get iconStyle() {
    return this._iconStyle;
  }

  attributeChangedCallback(name, oldVal, newVal) {
    if (name === "icon" || name === "icon-style") {
      this.classList.remove(oldVal);
      this.classList.add(newVal);
    }
  }
}

window.customElements.define("awesome-button", AwesomeButton, {
  extends: "button",
});
