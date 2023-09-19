/**
 * A plugin that offers anchors for other plugins to add their buttons to.
 * In order to facilitate an accessible user interface it is important that the tab-order of its
 * elements is in a sensible order. Different plugins add different interactible elements into
 * the DOM in an arbitrary order. This plugin exists to allow developers of plugins to add buttons
 * to the screen in a way that does not break the tab-order of their onscreen elements.
 *
 * To add an element to one of the four (three if the bottom right navigation is to be left alone) anchors,
 * simply place a button with the placeButton(...) method.
 *
 * In order to place a button you need to specify two things:
 * button: The DOM element that should be added to an anchor.
 * position: The anchor the DOM element should be added to: TOP_LEFT, TOP_RIGHT, BOTTOM_LEFT or BOTTOM_RIGHT.
 *
 * In addition, the plugins.css stylesheet allows for modification of the general look and feel of
 * all buttons added this way.
 * TODO: More responsive css for mobile users?
 */

class UIAnchorsPlugin {
  id = "ui-anchors";
  top_left_anchor = undefined;
  top_right_anchor = undefined;
  bottom_left_anchor = undefined;
  bottom_center_anchor = undefined;
  bottom_right_anchor = undefined;

  constructor() {
    this.id = "ui-anchors";
  }

  /**
   * Creates a new div with the given and the decker-anchor class.
   * @param {*} classname The class to be given to the new div.
   * @returns
   */
  createAnchor(classname) {
    let div = document.createElement("div");
    div.classList.add(classname);
    div.classList.add("decker-anchor");
    return div;
  }

  placeButton(button, position) {
    switch (position) {
      case "TOP_LEFT":
        this.addTopLeftButton(button);
        break;
      case "TOP_RIGHT":
        this.addTopRightButton(button);
        break;
      case "BOTTOM_LEFT":
        this.addBottomLeftButton(button);
        break;
      case "BOTTOM_CENTER":
        this.addBottomCenterButton(button);
        break;
      case "BOTTOM_RIGHT":
        this.addBottomRightButton(button);
        break;
      default:
        console.error("[PLUGIN MANAGER] [ERROR] No valid anchor specified.");
    }
  }

  addTopLeftButton(button) {
    this.top_left_anchor.appendChild(button);
  }

  addTopRightButton(button) {
    this.top_right_anchor.appendChild(button);
  }

  addBottomLeftButton(button) {
    this.bottom_left_anchor.appendChild(button);
  }

  addBottomCenterButton(button) {
    this.bottom_center_anchor.appendChild(button);
  }

  addBottomRightButton(button) {
    this.bottom_right_anchor.appendChild(button);
  }

  init(reveal) {
    this.top_left_anchor = this.createAnchor("top-left-anchor");
    this.top_right_anchor = this.createAnchor("top-right-anchor");
    this.bottom_left_anchor = this.createAnchor("bottom-left-anchor");
    this.bottom_center_anchor = this.createAnchor("bottom-center-anchor");
    this.bottom_right_anchor = this.createAnchor("bottom-right-anchor");

    let reveal_element = reveal.getViewportElement();
    reveal_element.prepend(this.top_left_anchor);
    reveal_element.appendChild(this.top_right_anchor);
    reveal_element.appendChild(this.bottom_left_anchor);
    reveal_element.appendChild(this.bottom_center_anchor);
    reveal_element.appendChild(this.bottom_right_anchor);
  }
}

let instance = new UIAnchorsPlugin();

export default instance;
