/**
 * A rewrite of the Slide Menu, inspired by the Reveal.js Slide Menu Plugin
 * made by Greg Denehy but leaving out a lot of configuration options and adding
 * accessibility features like using the inate polyfill, focus management and
 * proper aria-labels.
 *
 * @author Sebastian Hauer
 */

// import functionality for light/dark mode
import * as colorScheme from "../../js/color-scheme.js";
import * as flyingFocus from "../../flyingFocus/flying-focus.js";

class SlideMenu {
  reveal;
  open_button;
  position;
  menu;
  slide_list;

  constructor(position, reveal) {
    this.reveal = reveal;
    this.open_button = undefined;
    this.menu = {
      container: undefined,
      header: undefined,
      home_button: undefined,
      search_button: undefined,
      print_button: undefined,
      color_button: undefined,
      close_button: undefined,
      slide_list: undefined,
    };
    this.plugin_buttons = {
      container: undefined,
    };
    this.glass = undefined;
    this.position = position;
    this.localization = undefined;
  }

  get inert() {
    return this.menu.container.inert;
  }

  set inert(value) {
    this.menu.container.inert = !!value; //force cast to boolean
  }

  /**
   * Exposes the list items for other plugins.
   * @param {*} h
   * @param {*} v
   * @returns
   */
  getListItem(h, v) {
    let childNodes = this.menu.slide_list.childNodes;
    for (let i = 0; i < childNodes.length; i++) {
      if (childNodes[i].getAttribute("data-slide-h") == h) {
        if (v) {
          if (childNodes[i].getAttribute("data-slide-v") == v) {
            return childNodes[i];
          }
        } else {
          return childNodes[i];
        }
      }
    }
    return undefined;
  }

  getListItemByID(slideid) {
    let childNodes = this.menu.slide_list.childNodes;
    for (const child of childNodes) {
      if (child.getAttribute("data-slide-id") === slideid) {
        return child;
      }
    }
    return undefined;
  }

  /**
   * Exposes the slide list for other plugins.
   * @returns
   */
  getSlideList() {
    return this.menu.slide_list.childNodes;
  }

  /**
   * Toggles the inert attribute of the menu on or off.
   * @param {*} event
   */
  toggleMenu(event) {
    if (this.inert) {
      this.openMenu(event);
    } else {
      this.closeMenu(event);
    }
  }

  /**
   * Opens the menu by removing inert.
   * @param {*} event
   */
  openMenu(event) {
    if (this.inert) {
      this.inert = false;
      if (this.reveal.hasPlugin("ui-anchors")) {
        const anchors = this.reveal.getPlugin("ui-anchors");
        anchors.setInert(true);
      }
      if (document.documentElement.classList.contains("handout")) {
        document.getElementById("handout-container").inert = true;
      } else {
        this.reveal.getRevealElement().inert = true;
      }
      this.disableKeybinds();
      this.glass.classList.add("show");
      this.menu.home_button.removeAttribute("tabindex");
      this.menu.container.scroll(0, 0);
      if (event && event.detail === 0) {
        this.menu.close_button.focus();
        setTimeout(() => {
          flyingFocus.recenter();
        }, 500);
      }
      // scrolling the current slide into view now conflicts with the entire menu being scrollable
      // document.querySelector(".decker-menu .current-slide")?.scrollIntoView();
    }
  }

  /**
   * Closes the menu by adding inert.
   * @param {*} event
   */
  closeMenu(event) {
    if (!this.inert) {
      this.inert = true;
      if (this.reveal.hasPlugin("ui-anchors")) {
        const anchors = this.reveal.getPlugin("ui-anchors");
        anchors.setInert(false);
      }
      if (document.documentElement.classList.contains("handout")) {
        document.getElementById("handout-container").inert = false;
      } else {
        this.reveal.getRevealElement().inert = false;
      }
      for (const button of this.plugin_buttons.querySelectorAll("button")) {
        button.setAttribute("tabindex", -1);
      }
      this.enableKeybinds();
      this.glass.classList.remove("show");
      if (event && event.detail === 0) {
        setTimeout(() => this.open_button.focus());
      }
    }
  }

  /**
   * Navigate to index page
   */
  goToIndex() {
    let projectPath = Decker.meta.projectPath;
    if (projectPath.endsWith("/")) {
      window.location = projectPath + "index.html";
    } else {
      window.location = projectPath + "/index.html";
    }
  }

  /**
   * Toggles the searchbar of the searchbar plugin.
   */
  toggleSearchbar() {
    if (this.reveal.hasPlugin("search"))
      this.reveal.getPlugin("search").toggle();
  }

  /**
   * Reopens the tab with ?print-pdf to allow PDF printing.
   */
  printPDF() {
    if (window.electronApp) {
      let url = location.protocol + "//" + location.host + location.pathname;
      window.electronApp.printPDF(url);
    } else {
      if (confirm(this.localization.print_confirmation)) {
        let url =
          location.protocol +
          "//" +
          location.host +
          location.pathname +
          "?print-pdf";
        window.open(url, "_self");
      }
    }
  }

  disableKeybinds() {
    this.reveal.configure({ keyboard: false });
  }

  enableKeybinds() {
    // Do not enable keybinds again when we leaving in handout mode
    if (!document.documentElement.classList.contains("handout")) {
      this.reveal.configure({ keyboard: true });
    }
  }

  /**
   * Changes the way the up, down and escape keys work when a link inside the slide-list is focused.
   * @param {*} event The Keyboard Event
   */
  traverseList(event) {
    function changeFocus(to) {
      const currentItem = document.activeElement;
      currentItem.setAttribute("tabindex", "-1");
      to.setAttribute("tabindex", "0");
      to.focus();
    }
    if (!this.inert) {
      switch (event.code) {
        case "ArrowUp":
          if (
            document.activeElement &&
            document.activeElement.classList.contains("slide-link")
          ) {
            let parent = document.activeElement.parentElement;
            let target = undefined;
            if (parent.previousElementSibling) {
              //target the a inside the previous list item
              target = parent.previousElementSibling.firstElementChild;
            } else {
              // wrap around
              target = parent.parentElement.lastElementChild.firstElementChild;
            }
            changeFocus(target);
            event.preventDefault();
          }
          break;
        case "ArrowDown":
          if (
            document.activeElement &&
            document.activeElement.classList.contains("slide-link")
          ) {
            let parent = document.activeElement.parentElement;
            let target = undefined;
            if (parent.nextElementSibling) {
              //target the a inside the previous list item
              target = parent.nextElementSibling.firstElementChild;
            } else {
              // wrap around
              target = parent.parentElement.firstElementChild.firstElementChild;
            }
            changeFocus(target);
            event.preventDefault();
          }
          break;
        default:
      }
    }
  }

  traverseButtons(event) {
    function changeFocus(to) {
      const currentItem = document.activeElement;
      to.setAttribute("tabindex", "0");
      setTimeout(() => {
        to.focus();
        currentItem.setAttribute("tabindex", "-1");
      });
    }
    if (!document.activeElement) {
      return;
    }
    switch (event.key) {
      case "ArrowLeft":
      case "ArrowUp":
        {
          let target = document.activeElement;
          target = target.previousElementSibling;
          if (!target) {
            target = this.plugin_buttons.lastElementChild;
          }
          changeFocus(target);
          event.preventDefault();
        }
        break;
      case "ArrowRight":
      case "ArrowDown":
        {
          let target = document.activeElement;
          target = target.nextElementSibling;
          if (!target) {
            target = this.plugin_buttons.firstElementChild;
          }
          changeFocus(target);
          event.preventDefault();
        }
        break;
      default:
    }
  }

  /**
   * Instantiates the ui button that opens the menu.
   */
  initializeButton() {
    let template = document.createElement("template");
    template.innerHTML = String.raw`<button id="decker-menu-button" class="fa-button fas fa-bars" title="${this.localization.open_button_label}" aria-label="${this.localization.open_button_label}" aria-haspopup="menu" aria-controls="decker-menu">
    </button>`;

    let button = template.content.firstElementChild;
    button.addEventListener("click", (event) => this.toggleMenu(event));
    this.open_button = button;
  }

  /**
   * Instantiates the menu.
   */
  initializeSlideList() {
    let template = document.createElement("template");
    template.innerHTML = String.raw`<div class="slide-list-wrapper">
      <ul class="slide-list" role="menu" aria-label="${this.localization.navigation_list_label}"></ul>
    </div>`;
    let wrapper = template.content.firstElementChild;
    let list = wrapper.firstElementChild;
    let slides = document.querySelectorAll(".slides > section");
    slides.forEach((slide, h) => {
      let subslides = slide.querySelectorAll("section");
      if (subslides.length > 0) {
        subslides.forEach((subslide, v) => {
          let subitem = this.createListItem(subslide, h, v);
          list.appendChild(subitem);
        });
        return;
      }

      let item = this.createListItem(slide, h, undefined);
      list.appendChild(item);

      /* if there is only a single h1 element, 
      or if the slide has class .section, 
      then this is a separator slide. mark it in the menu */
      const h1 = slide.querySelector("h1");
      if (
        slide.classList.contains("section") ||
        (h1 && h1.parentElement.children.length == 1)
      ) {
        item.classList.add("separator-slide");
      }
    });
    this.menu.container.appendChild(wrapper);
    this.menu.slide_list = list;
  }

  /**
   * Creates a single list item for the slide list.
   * @param {*} slide
   * @param {*} h
   * @param {*} v
   * @returns
   */
  createListItem(slide, h, v) {
    let template = document.createElement("template");
    let title = this.getTitle(slide, "h1, h2, h3, h4, h5");
    title = `${h + 1}.${v !== undefined ? v + 1 : ""} ${title}`;
    template.innerHTML = String.raw`<li class="slide-list-item" data-slide-h="${h}" ${
      v !== undefined ? 'data-slide-v="' + v + '"' : ""
    } data-slide-id="${slide.id}" role="presentation">
      <a class="slide-link" href="#${
        slide.id
      }" target="_self" role="menuitem">${title}</a>
    </li>`;
    let item = template.content.firstElementChild;
    let link = item.firstElementChild;
    link.addEventListener("click", (event) => this.toggleMenu(event));
    return item;
  }

  /**
   * Tries to retrieve the title of a slide by various means.
   * @param {*} section
   * @param {*} selector
   * @returns
   */
  getTitle(section, selector) {
    let title = this.getTitleFromAttributesOrChildren(section, selector);
    if (!title) {
      title = this.getTitleFromSectionContent(section);
    }
    if (!title) {
      title = this.localization.no_title;
    }
    return title;
  }

  /**
   * Taken from Greg Denehy's Menu Plugin. Tries to find a title by going through
   * data-menu-title attribute of the given section, finding any element with the
   * class .menu-title or checking if there is an element with the custom selector
   * in the slide. If there is none, returns undefined.
   * @param {*} section
   * @param {*} selector
   * @returns The title of a slide or undefined.
   */
  getTitleFromAttributesOrChildren(section, selector) {
    let title =
      section.getAttribute("data-menu-title") ||
      section.getAttribute("menu-title");
    if (!title) {
      let element = section.querySelector(".menu-title");
      if (element) {
        title = element.textContent;
      }
    }
    if (!title && selector) {
      let element = section.querySelector(selector);
      if (element) {
        title = element.textContent;
      }
    }
    return title;
  }

  /**
   * Taken from Greg Denehy's Menu Plugin.
   * Searches the content of a slide for any text and uses that as a title.
   * @param {*} section
   * @returns
   */
  getTitleFromSectionContent(section) {
    let title = section.textContent.trim();
    if (title) {
      title =
        title
          .split("\n")
          .map(function (t) {
            return t.trim();
          })
          .join(" ")
          .trim()
          .replace(/^(.{16}[^\s]*).*/, "$1") // limit to 16 chars plus any consecutive non-whitespace chars (to avoid breaking words)
          .replace(/&/g, "&amp;")
          .replace(/</g, "&lt;")
          .replace(/>/g, "&gt;")
          .replace(/"/g, "&quot;")
          .replace(/'/g, "&#039;") + "...";
    }
    return title;
  }

  /**
   * Instantiates the whole menu and adds it to the DOM.
   */
  initializeMenu() {
    let template = document.createElement("template");
    template.innerHTML = String.raw`<nav class="decker-menu slide-in-left" id="decker-menu" role="menubar" aria-label="${this.localization.navigationmenu_label}" inert>
      <div class="menu-header">
        <button id="decker-menu-close-button" class="fa-button fas fa-times-circle" title="${this.localization.close_label}" aria-label="${this.localization.close_label}" role="menuitem">
        </button> 
        <div class="menu-header-button-group" role="group" aria-label="${this.localization.navigationmenu_label}">
          <button id="decker-menu-index-button" class="fa-button fas fa-home" title="${this.localization.home_button_label}" aria-label="${this.localization.home_button_label}" role="menuitem" tabindex="-1">
          </button>
          <button id="decker-menu-search-button" class="fa-button fas fa-search" title="${this.localization.search_button_label}" aria-label="${this.localization.search_button_label}" role="menuitem" tabindex="-1">
          </button>
          <button id="decker-menu-print-button" class="fa-button fas fa-print" title="${this.localization.print_pdf_label}" aria-label="${this.localization.print_pdf_label}" role="menuitem" tabindex="-1">
          </button>
          <button id="decker-menu-color-button" class="fa-button fas" title="${this.localization.toggle_colors_label}" aria-label="${this.localization.toggle_colors_label}" role="menuitem" tabindex="-1">
          </button>
        </div>
      </div>
     </nav>`;
    let container = template.content.firstElementChild;
    this.menu.container = container;

    this.menu.header = container.querySelector(".menu-header");

    /* Getting references */
    this.menu.home_button = container.querySelector(
      "#decker-menu-index-button"
    );
    this.menu.search_button = container.querySelector(
      "#decker-menu-search-button"
    );
    this.menu.print_button = container.querySelector(
      "#decker-menu-print-button"
    );
    this.menu.color_button = container.querySelector(
      "#decker-menu-color-button"
    );
    this.menu.close_button = container.querySelector(
      "#decker-menu-close-button"
    );

    this.plugin_buttons = container.querySelector(".menu-header-button-group");

    /* Attach callbacks */
    this.menu.home_button.addEventListener("click", (event) =>
      this.goToIndex()
    );
    this.menu.search_button.addEventListener("click", (event) =>
      this.toggleSearchbar()
    );
    this.menu.search_button.addEventListener("click", (event) =>
      this.closeMenu(event)
    );
    this.menu.print_button.addEventListener("click", (event) =>
      this.printPDF()
    );
    this.menu.print_button.addEventListener("click", (event) =>
      this.closeMenu(event)
    );
    this.menu.color_button.addEventListener("click", (event) => {
      if (this.menu.color_button.ariaDisabled) {
        return;
      }
      colorScheme.toggleColor();
    });
    this.menu.close_button.addEventListener("click", (event) =>
      this.closeMenu(event)
    );

    const colorSetting = window.Decker?.meta?.colorscheme;
    if (colorSetting == "light" || colorSetting == "dark")
      this.menu.color_button.ariaDisabled = true;

    this.initializeSlideList();
    this.menu.slide_list.addEventListener("keydown", (event) =>
      this.traverseList(event)
    );

    this.plugin_buttons.addEventListener("keydown", (event) => {
      this.traverseButtons(event);
    });

    /* Temporary Solution */
    this.glass = document.querySelector("#glass");
    if (!this.glass) {
      this.glass = document.createElement("div");
      this.glass.id = "glass";
      document.body.appendChild(this.glass);
    }

    /* Allow exit with ESC and focus with HOME and END */

    this.menu.container.addEventListener("keydown", (event) => {
      switch (event.key) {
        case "Escape":
          this.closeMenu(event);
          break;
        case "Home":
          setTimeout(() => this.menu.close_button.focus());
          event.preventDefault();
          break;
        case "End":
          setTimeout(() =>
            this.menu.slide_list.querySelector("[aria-current]").focus()
          );
          event.preventDefault();
          break;
      }
    });

    /* Trap Keyboard Focus */

    this.menu.slide_list.addEventListener("keydown", (event) => {
      if (event.key === "Tab" && !event.shiftKey) {
        event.preventDefault();
        setTimeout(() => this.menu.close_button.focus());
      }
    });

    this.menu.close_button.addEventListener("keydown", (event) => {
      if (event.key === "Tab" && event.shiftKey) {
        event.preventDefault();
        setTimeout(() =>
          this.menu.slide_list.querySelector("[aria-current]").focus()
        );
      }
    });

    this.glass.addEventListener("click", (event) => this.closeMenu(event));
  }

  addMenuButton(id, icon, title, callback) {
    const button = document.createElement("button");
    button.id = id;
    button.classList.add("fa-button", "fas", icon);
    button.title = title;
    button.setAttribute("aria-label", title);
    button.setAttribute("role", "menuitem");
    button.addEventListener("click", callback);
    const menuHeaderButtons = document.getElementsByClassName(
      "menu-header-buttons"
    );
    if (menuHeaderButtons.length > 0) {
      const container = menuHeaderButtons[0];
      container.insertBefore(button, this.menu.color_button);
    }
  }

  addPluginButton(id, icon, title, callback) {
    const button = document.createElement("button");
    button.id = id;
    button.classList.add("fa-button", "fas", icon);
    button.title = title;
    button.setAttribute("aria-label", title);
    button.setAttribute("role", "menuitem");
    button.setAttribute("tabindex", "-1");
    button.addEventListener("click", callback);
    this.plugin_buttons.appendChild(button);
    button.setLabel = function (value) {
      button.setAttribute("aria-label", value);
      button.title = value;
    };
    return button;
  }

  clearCurrentSlideMark() {
    let listItems = this.menu.slide_list.childNodes;
    for (let item of listItems) {
      item.querySelector("a").removeAttribute("aria-current");
      item.querySelector("a").setAttribute("tabindex", "-1");
    }
  }

  setCurrentSlideMark(slide) {
    if (!slide) {
      slide = this.reveal.getCurrentSlide();
    }
    let id = slide.id;
    let item = this.getListItemByID(id);
    if (item) {
      item.querySelector("a").setAttribute("aria-current", "page");
      item.querySelector("a").setAttribute("tabindex", "0");
    }
  }

  updateCurrentSlideMark(slide) {
    this.clearCurrentSlideMark();
    this.setCurrentSlideMark(slide);
  }
}

const printMode = /print-pdf/gi.test(window.location.search);

const plugin = () => {
  return {
    id: "decker-menu",
    getSlideList: undefined,
    getListItem: undefined,
    getListItemByID: undefined,
    updateCurrentSlideMark: undefined,
    addMenuButton: undefined,
    addPluginButton: undefined,
    inhibitKeyboard: undefined,
    init(reveal) {
      if (printMode) return;
      const menu = new SlideMenu("TOP_LEFT", reveal);
      menu.localization = {
        open_button_label: "Open Navigation Menu",
        home_button_label: "Go to Index Page",
        search_button_label: "Toggle Searchbar",
        print_pdf_label: "Print PDF",
        toggle_colors_label: "Toggle Color Mode",
        open_views_label: "Open View Menu",
        close_views_label: "Close View Menu",
        close_label: "Close Navigation Menu",
        no_title: "No Title",
        title: "Navigation",
        print_confirmation: "Leave presentation to export it to PDF?",
        index_confirmation: "Go back to index page?",
        navigationmenu_label: "Navigation Menu",
        navigation_list_label: "Slide List",
      };

      let lang = navigator.language;

      if (lang === "de") {
        menu.localization = {
          open_button_label: "Navigationsmenu öffnen",
          home_button_label: "Zurück zur Materialübersicht",
          search_button_label: "Suchleiste umschalten",
          print_pdf_label: "Als PDF drucken",
          toggle_colors_label: "Farbmodus umschalten",
          open_views_label: "Anzeigemenu öffnen",
          close_views_label: "Anzeigemenu schließen",
          close_label: "Navigationsmenu schließen",
          no_title: "Kein Titel",
          title: "Navigation",
          print_confirmation: "Seite verlassen, um sie als PDF zu exportieren?",
          index_confirmation: "Zurück zur Index-Seite gehen?",
          navigationmenu_label: "Navigationsmenu",
          navigation_list_label: "Folienliste",
        };
      }

      menu.initializeButton();
      menu.initializeMenu();

      document.body.prepend(menu.menu.container);

      this.getSlideList = () => {
        return menu.getSlideList();
      };

      this.getListItem = (h, v) => {
        return menu.getListItem(h, v);
      };

      this.getListItemByID = (id) => {
        return menu.getListItemByID(id);
      };

      this.addMenuButton = (id, icon, title, callback) => {
        menu.addMenuButton(id, icon, title, callback);
      };

      this.addPluginButton = (id, icon, title, callback) => {
        return menu.addPluginButton(id, icon, title, callback);
      };

      this.updateCurrentSlideMark = (slide) => {
        menu.updateCurrentSlideMark(slide);
      };

      this.slide_list_container = menu;

      if (!reveal.hasPlugin("ui-anchors")) {
        console.log("no decker ui anchor plugin loaded");
        return;
      }
      let anchors = reveal.getPlugin("ui-anchors");
      anchors.placeButton(menu.open_button, menu.position);
      reveal.addEventListener("slidechanged", (event) => {
        const currentSlide = event.currentSlide;
        menu.updateCurrentSlideMark(currentSlide);
      });
      reveal.addEventListener("ready", () => {
        this.updateCurrentSlideMark();
      });
    },
  };
};

export default plugin;
