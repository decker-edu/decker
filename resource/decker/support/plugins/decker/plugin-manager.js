/**
 * A plugin that manages other plugins.
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

    prepareAnchor(classname) {
        let template = document.createElement("template");
        template.innerHTML = String.raw
        `<div class="${classname} decker-anchor">
        </div>`;
        let div = template.content.firstElementChild;
        return div;
    }

    placeButton(button, position) {
        switch(position) {
            case "TOP_LEFT": this.addTopLeftButton(button); break;
            case "TOP_RIGHT": this.addTopRightButton(button); break;
            case "BOTTOM_LEFT": this.addBottomLeftButton(button); break;
            case "BOTTOM_RIGHT": this.addBottomRightButton(button); break;
            default: console.error("[PLUGIN MANAGER] [ERROR] No valid anchor specified.");
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

    addBottomRightButton(button) {
        this.bottom_right_anchor.appendChild(button);
    }

    init(reveal) {
        this.reveal = reveal;
        this.top_left_anchor = this.prepareAnchor("top-left-anchor");
        this.top_right_anchor = this.prepareAnchor("top-right-anchor");
        this.bottom_left_anchor = this.prepareAnchor("bottom-left-anchor");
        this.bottom_right_anchor = this.prepareAnchor("bottom-right-anchor");

        let reveal_element = document.querySelector(".reveal");
        let target = reveal_element;
        if(reveal_element.parent) {
            target = reveal_element.parent;
        }
        target.prepend(this.top_left_anchor);
        target.appendChild(this.top_right_anchor);
        target.appendChild(this.bottom_left_anchor);
        target.appendChild(this.bottom_right_anchor);
    }
}

let instance = new DeckerPlugins();

export default instance;