/*!
 * Handles finding a text string anywhere in the slides and showing the next occurrence to the user
 * by navigating to that slide and highlighting it.
 *
 * @author Jon Snyder <snyder.jon@gmail.com>, February 2013
 *
 * Several adjustments and fixes by Sebastian Hauer, Mario Botsch
 */

const lang_de = {
  search: "Suche ...",
  searchinslides: "In den Folien suchen",
  prevResult: "Vorherige Übereinstimmung",
  nextResult: "Nächste Übereinstimmung",
  close: "Suchdialog schließen",
  searchinputfield:
    "In den Folien suchen. Eingabe drücken, um Suche zu starten.",
  of: "von",
  matches: "Übereinstimmungen",
  noMatches: "Keine Übereinstimmungen",
};

const lang_en = {
  search: "Search ...",
  searchinslides: "Search in slides",
  prevResult: "Previous match",
  nextResult: "Next match",
  close: "Close search dialog",
  searchinputfield: "Search in slides",
  of: "of",
  matches: "Matches",
  noMatches: "No Matches",
};

let l10n = navigator.language === "de" ? lang_de : lang_en;

const Plugin = () => {
  // The reveal.js instance this plugin is attached to
  let deck;

  let searchElement;
  let searchInput;
  let inputLabel;
  let searchPrev;
  let searchNext;
  let searchLabel;
  let searchClose;

  let matchedSlides;
  let currentMatchedIndex;
  let searchboxDirty;
  let hilitor;

  function render() {
    searchElement = document.createElement("div");
    searchElement.id = "searchbox";
    searchElement.innerHTML = `<div>
      <label id="searchinputlabel" for="searchinput">${l10n.searchinslides}</label>
      <div role="search" id="searchrow" style="display:flex; align-items:center; gap:0.5em;">
        <i class="fa-button fas fa-search"></i>
        <input type="search" id="searchinput"></input>
        <span id="searchamount">0 / 0</span>
        <span id="searchlabel" aria-live="polite">${l10n.noMatches}</span>
        <button id="searchprev" class="fas fa-chevron-up" title="${l10n.prevResult}" aria-label="${l10n.prevResult}"></button>
        <button id="searchnext" class="fas fa-chevron-down" title="${l10n.nextResult}" aria-label="${l10n.nextResult}"></button>
        <button id="searchclose" class="fas fa-xmark" title="${l10n.close}" aria-label="${l10n.close}"></button>
      </div>
    </div>`;

    searchInput = searchElement.querySelector("#searchinput");
    searchInput.placeholder = l10n.search;

    searchPrev = searchElement.querySelector("#searchprev");
    searchPrev.addEventListener("click", () => {
      if (!searchPrev.hasAttribute("aria-disabled")) previousResult();
    });

    searchNext = searchElement.querySelector("#searchnext");
    searchNext.addEventListener("click", () => {
      if (!searchNext.hasAttribute("aria-disabled")) nextResult();
    });

    searchClose = searchElement.querySelector("#searchclose");
    searchClose.addEventListener("click", closeSearch);

    if (!deck.hasPlugin("ui-anchors")) {
      console.error("no decker ui anchor plugin loaded");
    } else {
      deck.getPlugin("ui-anchors").placeButton(searchElement, "TOP_LEFT");
    }

    searchInput.addEventListener(
      "keyup",
      function (event) {
        if (event.key === "Enter") {
          event.preventDefault();
          doSearch();
          searchboxDirty = false;
        } else {
          searchboxDirty = true;
        }
      },
      false
    );

    searchElement.addEventListener(
      "keyup",
      function (event) {
        if (event.key === "Escape") closeSearch();
      },
      false
    );

    closeSearch();
  }

  function openSearch() {
    if (!searchElement) render();

    searchElement.style.display = "flex";
    searchInput.focus();
    searchInput.select();
  }

  function closeSearch() {
    if (!searchElement) render();

    searchElement.style.display = "none";
    if (hilitor) hilitor.remove();
    setLabelToNoMatches();
    disableButtons();
    matchedSlides = null;
    currentMatchedIndex = -1;
  }

  function toggleSearch() {
    if (!searchElement) render();

    if (searchElement.style.display !== "flex") {
      openSearch();
    } else {
      closeSearch();
    }
  }

  /**
   * Update text of labels when no matches were found and disable the next and prev buttons.
   */
  function setLabelToNoMatches() {
    const amountSpan = searchElement.querySelector("#searchamount");
    const amountLabel = searchElement.querySelector("#searchlabel");
    disableButtons();
    amountSpan.innerText = `0 / 0`;
    amountLabel.innerText = `${l10n.noMatches}`;
  }

  /**
   * Update text of labels when matches were found and enable next and prev buttons.
   */
  function updateLabels(matchIndex) {
    const amountSpan = searchElement.querySelector("#searchamount");
    const amountLabel = searchElement.querySelector("#searchlabel");
    enableButtons();
    amountSpan.innerText = `${matchIndex + 1} / ${matchedSlides.length}`;
    amountLabel.innerText = `${matchIndex + 1}. ${l10n.of} ${
      matchedSlides.length
    } ${l10n.matches}`;
  }

  function disableButtons() {
    searchPrev.setAttribute("aria-disabled", "true");
    searchNext.setAttribute("aria-disabled", "true");
    /* All the styling in this plugin is done by hand and not a css file which would make this much easier ... */
    searchPrev.style.color = "var(--icon-disabled-color)";
    searchNext.style.color = "var(--icon-disabled-color)";
  }

  function enableButtons() {
    searchPrev.removeAttribute("aria-disabled");
    searchNext.removeAttribute("aria-disabled");
    searchPrev.style.color = "var(--icon-active-color)";
    searchNext.style.color = "var(--icon-active-color)";
  }

  function nextResult() {
    if (matchedSlides && matchedSlides.length > 0) {
      let matchIndex = currentMatchedIndex + 1;
      //navigate to the next slide that has the keyword, wrapping to the first if necessary
      if (matchedSlides.length && matchIndex >= matchedSlides.length) {
        matchIndex = 0;
      }
      if (matchIndex < matchedSlides.length) {
        deck.slide(matchedSlides[matchIndex].h, matchedSlides[matchIndex].v);
        updateLabels(matchIndex);
        currentMatchedIndex = matchIndex;
      }
    } else {
      setLabelToNoMatches();
    }
  }

  function previousResult() {
    if (matchedSlides && matchedSlides.length > 0) {
      let matchIndex = currentMatchedIndex - 1;
      //navigate to the next slide that has the keyword, wrapping to the first if necessary
      if (matchedSlides.length && matchIndex < 0) {
        matchIndex = matchedSlides.length - 1;
      }
      if (matchIndex >= 0) {
        deck.slide(matchedSlides[matchIndex].h, matchedSlides[matchIndex].v);
        updateLabels(matchIndex);
        currentMatchedIndex = matchIndex;
      }
    } else {
      setLabelToNoMatches();
    }
  }

  function doSearch() {
    //if there's been a change in the search term, perform a new search:
    if (searchboxDirty) {
      var searchstring = searchInput.value;

      if (searchstring === "") {
        if (hilitor) hilitor.remove();
        matchedSlides = null;
      } else {
        //find the keyword amongst the slides
        hilitor = new Hilitor(".slides");
        matchedSlides = hilitor.apply(searchstring);
        currentMatchedIndex = -1;
      }
    }
    nextResult();
  }

  // Original JavaScript code by Chirp Internet: www.chirp.com.au
  // Please acknowledge use of this code by including this header.
  // 2/2013 jon: modified regex to display any match, not restricted to word boundaries.
  function Hilitor(selector, tag) {
    var targetNode = document.querySelector(selector) || document.body;
    var hiliteTag = tag || "EM";
    var skipTags = new RegExp("^(?:" + hiliteTag + "|SCRIPT|FORM)$");
    var colors = ["#ff6", "#a0ffff", "#9f9", "#f99", "#f6f"];
    var wordColor = [];
    var colorIdx = 0;
    var matchRegex = "";
    var matchingSlides = [];

    this.setRegex = function (input) {
      input = input.replace(/^[^\w]+|[^\w]+$/g, "").replace(/[^\w'-]+/g, "|");
      matchRegex = new RegExp("(" + input + ")", "i");
    };

    this.getRegex = function () {
      const regex = matchRegex
        .toString()
        .replace(/^\/\\b\(|\)\\b\/i$/g, "")
        .replace(/\|/g, " ");
      return regex;
    };

    // recursively apply word highlighting
    this.hiliteWords = function (node) {
      if (node == undefined || !node) return;
      if (!matchRegex) return;
      if (skipTags.test(node.nodeName)) return;

      if (node.hasChildNodes()) {
        for (var i = 0; i < node.childNodes.length; i++)
          this.hiliteWords(node.childNodes[i]);
      }
      if (node.nodeType == 3) {
        // NODE_TEXT
        var nv, regs;
        if ((nv = node.nodeValue) && (regs = matchRegex.exec(nv))) {
          //find the slide's section element and save it in our list of matching slides
          var secnode = node;
          while (secnode != null && secnode.nodeName != "SECTION") {
            secnode = secnode.parentNode;
          }

          var slideIndex = deck.getIndices(secnode);
          var slidelen = matchingSlides.length;
          var alreadyAdded = false;
          for (var i = 0; i < slidelen; i++) {
            if (
              matchingSlides[i].h === slideIndex.h &&
              matchingSlides[i].v === slideIndex.v
            ) {
              alreadyAdded = true;
            }
          }
          if (!alreadyAdded) {
            matchingSlides.push(slideIndex);
          }

          if (!wordColor[regs[0].toLowerCase()]) {
            wordColor[regs[0].toLowerCase()] =
              colors[colorIdx++ % colors.length];
          }

          var match = document.createElement(hiliteTag);
          match.appendChild(document.createTextNode(regs[0]));
          match.style.backgroundColor = wordColor[regs[0].toLowerCase()];
          match.style.fontStyle = "inherit";
          match.style.color = "#000";

          var after = node.splitText(regs.index);
          after.nodeValue = after.nodeValue.substring(regs[0].length);
          node.parentNode.insertBefore(match, after);
        }
      }
    };

    // remove highlighting
    this.remove = function () {
      var arr = document.getElementsByTagName(hiliteTag);
      var el;
      // destroy hiliteTag span elements and re-merge the text nodes
      while (arr.length && (el = arr[0])) {
        const parent = el.parentNode;
        parent.replaceChild(el.firstChild, el);
        parent.normalize();
      }
    };

    // start highlighting at target node
    this.apply = function (input) {
      if (input == undefined || !input) return;
      this.remove();
      this.setRegex(input);
      this.hiliteWords(targetNode);
      return matchingSlides;
    };
  }

  return {
    id: "search",

    init: (reveal) => {
      deck = reveal;

      // MARIO: CTRL/CMD + F (instead of CTRL+SHIFT+F)
      deck.registerKeyboardShortcut("CTRL + F", "Search");
      document.addEventListener(
        "keydown",
        function (event) {
          if (event.key == "f" && (event.ctrlKey || event.metaKey)) {
            // If Handout Mode is active do a normal document search
            if (deck.hasPlugin("handout")) {
              const handoutPlugin = deck.getPlugin("handout");
              if (handoutPlugin.isActive()) {
                return;
              }
            }
            event.preventDefault();
            toggleSearch();
          }
        },
        false
      );
    },

    open: openSearch,

    // MARIO: also export toggleSearch to trigger it from menu
    toggle: toggleSearch,
  };
};

export default Plugin;
