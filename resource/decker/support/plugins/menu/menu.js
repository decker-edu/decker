/**
 * A rewrite of the Slide Menu, inspired by the Reveal.js Slide Menu Plugin
 * made by Greg Denehy but leaving out a lot of configuration options and adding
 * accessibility features like using the inate polyfill, focus management and
 * proper aria-labels.
 *
 * @author Sebastian Hauer
 */

class SlideMenu {
  id;
  reveal;
  config;
  open_button;
  position;
  menu;
  slide_list;

  constructor(position) {
    this.id = "decker-menu";
    this.reveal = undefined;
    this.config = undefined;
    this.open_button = undefined;
    this.menu = {
      container: undefined,
      search_button: undefined,
      pdf_button: undefined,
      settings_button: undefined,
      close_button: undefined,
      slide_list: undefined,
    };
    this.settings = {
      container: undefined,
      fragments_toggle: undefined,
      annotations_toggle: undefined,
      color_choice: undefined,
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
      this.reveal.getRevealElement().inert = true;
      this.disableKeybinds();
      this.glass.classList.add("show");
      if (event && event.detail === 0) {
        this.menu.search_button.focus();
      }
    }
  }

  /**
   * Closes the menu by adding inert.
   * @param {*} event
   */
  closeMenu(event) {
    if (!this.inert) {
      this.closeSettings();
      this.inert = true;
      this.reveal.getRevealElement().inert = false;
      this.enableKeybinds();
      this.glass.classList.remove("show");
      if (event && event.detail === 0) {
        this.open_button.focus();
      }
    }
  }

  toggleSettings() {
    if (this.settings.container.inert) {
      this.openSettings();
    } else {
      this.closeSettings();
    }
  }

  openSettings() {
    this.settings.container.inert = false;
    this.menu.slide_list.inert = true;
    this.menu.settings_button.setAttribute(
      "title",
      this.localization.close_settings_label
    );
    this.menu.settings_button.setAttribute(
      "aria-label",
      this.localization.close_settings_label
    );
  }

  closeSettings() {
    this.settings.container.inert = true;
    this.menu.slide_list.inert = false;
    this.menu.settings_button.setAttribute(
      "title",
      this.localization.open_settings_label
    );
    this.menu.settings_button.setAttribute(
      "aria-label",
      this.localization.open_settings_label
    );
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
    this.reveal.configure({ keyboard: true });
  }

  /**
   * Enables or disables the fragmentation of slides.
   */
  toggleFragments() {
    let animations = this.reveal.getConfig().fragments;
    this.reveal.configure({ fragments: !animations });
    if (!animations) {
      this.settings.fragments_toggle.checked = true;
      this.settings.fragments_toggle.setAttribute("aria-checked", "true");
    } else {
      this.settings.fragments_toggle.checked = false;
      this.settings.fragments_toggle.setAttribute("aria-checked", "false");
    }
  }

  /**
   * If there is a status field to announce changes to the GUI then use that to announce
   * changes.
   * TODO: Test if this is actually necessary.
   * @param {*} text
   */
  announceStatus(text) {
    if (this.reveal.hasPlugin("a11y-status")) {
      let status = this.reveal.getPlugin("a11y-status");
      status.announce(text);
    } else {
      console.log("No a11y-status plugin found.");
    }
  }

  /**
   * Stops the default functionality of moving up or down the scrollbar of the slide wrapper div.
   * @param {*} event The Keyboard Event
   */
  ignoreTraversalKeys(event) {
    if (
      !this.inert &&
      (event.code == "Escape" ||
        event.code == "ArrowUp" ||
        event.code == "ArrowDown")
    ) {
      event.preventDefault();
    }
  }

  /**
   * Changes the way the up, down and escape keys work when a link inside the slide-list is focused.
   * @param {*} event The Keyboard Event
   */
  traverseList(event) {
    if (!this.inert) {
      switch (event.code) {
        case "Escape":
          //              event.stopImmediatePropagation();
          this.closeMenu();
          break;
        case "ArrowUp":
          if (
            document.activeElement &&
            document.activeElement.classList.contains("tile")
          ) {
            event.preventDefault();
            //                event.stopImmediatePropagation();
            this.menu.slide_list.lastElementChild.firstElementChild.focus();
          }
          if (
            document.activeElement &&
            document.activeElement.classList.contains("slide-link")
          ) {
            //                  event.stopImmediatePropagation();
            let parent = document.activeElement.parentElement;
            let target = undefined;
            if (parent.previousElementSibling) {
              //target the a inside the previous list item
              target = parent.previousElementSibling.firstElementChild;
            } else {
              // wrap around
              target = parent.parentElement.lastElementChild.firstElementChild;
            }
            setTimeout(() => target.focus());
          }
          break;
        case "ArrowDown":
          if (
            document.activeElement &&
            document.activeElement.classList.contains("tile")
          ) {
            event.preventDefault();
            //            event.stopImmediatePropagation();
            this.menu.slide_list.firstElementChild.firstElementChild.focus();
          }
          if (
            document.activeElement &&
            document.activeElement.classList.contains("slide-link")
          ) {
            //              event.stopImmediatePropagation();
            let parent = document.activeElement.parentElement;
            let target = undefined;
            if (parent.nextElementSibling) {
              //target the a inside the previous list item
              target = parent.nextElementSibling.firstElementChild;
            } else {
              // wrap around
              target = parent.parentElement.firstElementChild.firstElementChild;
            }
            setTimeout(() => target.focus());
          }
          break;
        default:
      }
    }
  }

  /**
   * Instantiates the ui button that opens the menu.
   */
  initializeButton() {
    let template = document.createElement("template");
    template.innerHTML = String.raw`<button id="decker-menu-button" class="fa-button fas fa-bars" title="${this.localization.open_button_label}" aria-label="${this.localization.open_button_label}">
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
    template.innerHTML = String.raw`<div class="slide-list-wrapper" tabindex="-1">
      <ul class="slide-list" tabindex="-1"></ul>
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
      var item = this.createListItem(slide, h, undefined);
      list.appendChild(item);
    });
    wrapper.addEventListener("keydown", (event) =>
      this.ignoreTraversalKeys(event)
    );
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
    }>
      <a class="slide-link" href="#/${h}/${
      v !== undefined ? "/" + v : ""
    }" target="_self">${title}</a>
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
    let title = section.getAttribute("data-menu-title");
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
    template.innerHTML = String.raw`<div class="decker-menu slide-in-left" id="decker-menu" inert>
      <div class="menu-header">
        <div class="menu-header-buttons">
          <button id="decker-menu-close-button" class="fa-button fas fa-times-circle" title="${this.localization.close_label}" aria-label="${this.localization.close_label}">
          </button>
          <button id="decker-menu-search-button" class="fa-button fas fa-search" title="${this.localization.search_button_label}" aria-label="${this.localization.search_button_label}">
          </button>
          <button id="decker-menu-print-button" class="fa-button fas fa-print" title="${this.localization.print_pdf_label}" aria-label="${this.localization.print_pdf_label}">
          </button>
          <button id="decker-menu-settings-button" class="fa-button fas fa-cog" title="${this.localization.open_settings_label}" aria-label="${this.localization.open_settings_label}">
          </button>
        </div>
        <div id="decker-menu-title">
          <span>${this.localization.title}</span>
        </div>
      </div>
     </div>`;
    let container = template.content.firstElementChild;
    this.menu.container = container;

    /* Getting references */
    this.menu.search_button = container.querySelector(
      "#decker-menu-search-button"
    );
    this.menu.pdf_button = container.querySelector("#decker-menu-print-button");
    this.menu.settings_button = container.querySelector(
      "#decker-menu-settings-button"
    );
    this.menu.close_button = container.querySelector(
      "#decker-menu-close-button"
    );

    /* Attach callbacks */
    this.menu.search_button.addEventListener("click", (event) =>
      this.toggleSearchbar()
    );
    this.menu.search_button.addEventListener("click", (event) =>
      this.closeMenu(event)
    );
    this.menu.pdf_button.addEventListener("click", (event) => this.printPDF());
    this.menu.pdf_button.addEventListener("click", (event) =>
      this.closeMenu(event)
    );
    this.menu.settings_button.addEventListener("click", (event) =>
      this.toggleSettings()
    );
    this.menu.close_button.addEventListener("click", (event) =>
      this.closeMenu(event)
    );

    this.initializeSlideList();
    this.initializeSettingsMenu();
    this.menu.container.addEventListener("keydown", (event) =>
      this.traverseList(event)
    );

    /* Temporary Solution */
    this.glass = document.querySelector("#glass");
    if (!this.glass) {
      this.glass = document.createElement("div");
      this.glass.id = "glass";
      document.body.appendChild(this.glass);
    }
    this.glass.addEventListener("click", (event) => this.closeMenu(event));
  }

  toggleAnnotations() {
    document.documentElement.classList.toggle("hide-annotations");
  }

  toggleColorMode(mode) {
    if (mode === "dark") {
      localStorage.setItem("color-mode", "dark");
      document.documentElement.classList.add("dark");
      document.documentElement.classList.remove("light");
    }
    if (mode === "light") {
      localStorage.setItem("color-mode", "light");
      document.documentElement.classList.add("light");
      document.documentElement.classList.remove("dark");
    }
    if (mode === "system") {
      localStorage.removeItem("color-mode");
      document.documentElement.classList.remove("dark");
      document.documentElement.classList.remove("light");
    }
  }

  getColorModePreference() {
    let match = window.matchMedia("(prefers-color-scheme: dark)");
    let system = match.matches ? "dark" : "light";
    let storage = localStorage.getItem("color-mode");
    let choice = storage ? storage : system;
    return choice;
  }

  initializeColorModePreference() {
    let storage = localStorage.getItem("color-mode");
    if (storage) {
      this.toggleColorMode(storage);
    }
  }

  initializeSettingsMenu() {
    let animations = this.reveal.getConfig().fragments;
    let mode = this.getColorModePreference();
    let template = document.createElement("template");
    template.innerHTML = String.raw`<div class="menu-settings" inert>
    <div class="settings-item">
      <div class="settings-toggle-wrapper">
        <label for="setting-toggle-fragments" class="settings-toggle" aria-label="${
          this.localization.toggle_fragments_label
        }">
          <input id="setting-toggle-fragments" class="settings-toggle-checkbox" type="checkbox" ${
            animations ? "checked" : ""
          } />
          <span class="slider round"></span>
        </label>
        <label for="setting-toggle-fragments">${
          this.localization.toggle_fragments_label
        }</label>
      </div>
    </div>
    <div class="settings-item">
      <div class="settings-toggle-wrapper">
        <label for="setting-toggle-annotations" class="settings-toggle" aria-label="${
          this.localization.toggle_annotations_label
        }">
          <input id="setting-toggle-annotations" class="settings-toggle-checkbox" type="checkbox" checked />
          <span class="slider round"></span>
        </label>
        <label for="setting-toggle-annotations">${
          this.localization.toggle_annotations_label
        }</label>
      </div>
    </div>
    <div class="settings-item">
      <div class="settings-choice-wrapper">
        <fieldset id="color-choice">
          <legend>${this.localization.choose_color_label}</legend>
          <div class="choice-pair">
            <input id="system-color-radio" type="radio" name="color-mode" value="system" aria-label="${
              this.localization.system_color_choice
            }" ${mode === "system" ? "checked" : ""}>
            <label for="system-color-radio">${
              this.localization.system_color_choice
            }</label>
          </div>
          <div class="choice-pair">
            <input id="light-color-radio" type="radio" name="color-mode" value="light" aria-label="${
              this.localization.light_color_choice
            }" ${mode === "light" ? "checked" : ""}>
            <label for="light-color-radio">${
              this.localization.light_color_choice
            }</label>
          </div>
          <div class="choice-pair">
            <input id="dark-color-radio" type="radio" name="color-mode" value="dark" aria-label="${
              this.localization.dark_color_choice
            }" ${mode === "dark" ? "checked" : ""}>
            <label for="dark-color-radio">${
              this.localization.dark_color_choice
            }</label>
          </div>
        </fieldset>
      </div>
    </div>
  </div>`;
    this.settings.container = template.content.firstElementChild;
    this.menu.container.appendChild(this.settings.container);
    this.settings.fragments_toggle = this.settings.container.querySelector(
      "#setting-toggle-fragments"
    );
    this.settings.fragments_toggle.addEventListener("change", (event) =>
      this.toggleFragments()
    );
    this.settings.color_choice =
      this.settings.container.querySelector("#color-choice");
    this.settings.color_choice.addEventListener("change", (event) => {
      this.toggleColorMode(event.target.value);
    });
    this.settings.annotations_toggle =
      this.settings.container.querySelector("#setting-toggle-annotations");
    this.settings.annotations_toggle.addEventListener("change", (event) =>
      this.toggleAnnotations(event.target.checked)
    );
  }

  init(reveal) {
    this.reveal = reveal;
    this.config = reveal.getConfig();

    this.localization = {
      open_button_label: "Open Navigation Menu",
      search_button_label: "Toggle Searchbar",
      print_pdf_label: "Print PDF",
      open_settings_label: "Open Settings",
      close_settings_label: "Close Settings",
      toggle_fragments_label: "Show Slide Fragments",
      choose_color_label: "Choose color mode",
      system_color_choice: "System Default",
      light_color_choice: "Light Mode",
      dark_color_choice: "Dark Mode",
      toggle_annotations_label: "Show handwritten notes",
      close_label: "Close Navigation Menu",
      no_title: "No Title",
      title: "Navigation",
      print_confirmation: "Leave/Reload presentation to export PDF?",
    };

    let lang = navigator.language;

    if (lang === "de") {
      this.localization = {
        open_button_label: "Navigationsmenu öffnen",
        search_button_label: "Suchleiste umschalten",
        print_pdf_label: "Als PDF drucken",
        open_settings_label: "Einstellungen öffnen",
        close_settings_label: "Einstellungen schließen",
        toggle_fragments_label: "Folienfragmente anzeigen",
        choose_color_label: "Farbschema auswählen",
        system_color_choice: "Systemeinstellung",
        light_color_choice: "Helles Farbschema",
        dark_color_choice: "Dunkles Farbschema",
        toggle_annotations_label: "Notizen einblenden",
        close_label: "Navigationsmenu schließen",
        no_title: "Kein Titel",
        title: "Navigation",
        print_confirmation:
          "Die Seite neuladen / verlassen um sie als PDF zu exportieren?",
      };
    }

    this.initializeButton();
    this.initializeMenu();
    this.initializeColorModePreference();

    document.body.appendChild(this.menu.container);

    if (!this.reveal.hasPlugin("ui-anchors")) {
      console.log("no decker ui anchor plugin loaded");
      return;
    }
    let anchors = this.reveal.getPlugin("ui-anchors");
    anchors.placeButton(this.open_button, this.position);
  }
}

let instance = new SlideMenu("TOP_LEFT");

export default instance;
