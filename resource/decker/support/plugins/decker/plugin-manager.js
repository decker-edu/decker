/**
 * A plugin that manages other plugins.
 * In order to facilitate an accessible user interface it is paramount that the tab-order of its
 * elements is in reading order. Different plugins add different interactible elements into the DOM
 * in an arbitrary order. This plugin exists to allow developers of plugins to add buttons to the
 * screen in a way that does not break the tab-order of their onscreen elements.
 * 
 * To add an element to one of the four (three if the bottom right navigation is to be left alone) anchors,
 * simply register a plugin with the registerPlugin(...) method.
 * 
 * The registered plugin object needs two attributes:
 * decker_button: The DOM element that should be added to an anchor.
 * decker_anchor: The anchor the DOM element should be added to: TOP_LEFT, TOP_RIGHT, BOTTOM_LEFT or BOTTOM_RIGHT.
 * 
 * In addition, the plugins.css stylesheet allows for modification of the general look and feel of all buttons added this way.
 * TODO: More responsive css for mobile users?
 */

class DeckerPlugins {
    id = "decker-plugins";
    reveal = undefined; //reference to the reveal object received in init
    top_left_anchor = undefined;
    top_right_anchor = undefined;
    bottom_left_anchor = undefined;
    bottom_right_anchor = undefined;
    
    constructor() {
        this.id = "decker-plugins";
    }

    init(reveal) {
        this.reveal = reveal;
        this.top_left_anchor = this.prepareAnchor("top-left-anchor");
        this.top_right_anchor = this.prepareAnchor("top-right-anchor");
        this.bottom_left_anchor = this.prepareAnchor("bottom-left-anchor");
        this.bottom_right_anchor = this.prepareAnchor("bottom-right-anchor");

        let reveal_element = document.querySelector(".reveal");
        reveal_element.appendChild(this.top_left_anchor);
        reveal_element.appendChild(this.top_right_anchor);
        reveal_element.appendChild(this.bottom_left_anchor);
        reveal_element.appendChild(this.bottom_right_anchor);
    }

    prepareAnchor(classname) {
        let template = document.createElement("template");
        template.innerHTML = String.raw
        `<div class="${classname} decker-anchor">
        </div>`;
        let div = template.content.firstChild;
        return div;
    }

    registerPlugin(plugin) {
        let button = plugin.decker_button;
        let anchor = plugin.decker_anchor;
        switch(anchor) {
            case "TOP_LEFT": this.addTopLeftButton(button); break;
            case "TOP_RIGHT": this.addTopRightButton(button); break;
            case "BOTTOM_LEFT": this.addBottomLeftButton(button); break;
            default: console.log("[PLUGIN MANAGER] [ERROR] No valid anchor specified.");
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
}

let instance = new DeckerPlugins();

export default instance;