/*!
 * Handles finding a text string anywhere in the slides and showing the
 * next occurrence to the user by navigating to that slide and highlighting it.
 * @author Jon Snyder <snyder.jon@gmail.com>, February 2013
 *
 * Original hilitor code by Chirp Internet: www.chirp.com.au
 * Please acknowledge use of this code by including this header.
 * 2/2013 jon: modified regex to display any match, not restricted to word boundaries.
 *
 * Several adjustments for Decker, some bug fixes, and port to CustomHighlights API
 * by Sebastian Hauer and Mario Botsch
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

const l10n = navigator.language === "de" ? lang_de : lang_en;

const Plugin = () => {
  let deck;

  let searchElement;
  let searchInput;
  let searchPrev;
  let searchNext;
  let searchClose;
  let amountSpan;
  let amountLabel;

  let matchedSlides;
  let matchIndex;
  let searchboxDirty;
  let searchString;

  function createDialog() {
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

    amountSpan = searchElement.querySelector("#searchamount");
    amountLabel = searchElement.querySelector("#searchlabel");

    if (!deck.hasPlugin("ui-anchors")) {
      console.error("no decker ui anchor plugin loaded");
    } else {
      deck.getPlugin("ui-anchors").placeButton(searchElement, "TOP_LEFT");
    }

    searchInput.addEventListener(
      "keyup",
      function (event) {
        const input = searchInput.value.trim().toLowerCase();
        if (event.key === "Enter") {
          event.preventDefault();
          // do new search
          if (searchboxDirty) {
            if (input === "") {
              clearSearch();
            } else {
              doSearch(input);
              matchIndex = -1;
              searchboxDirty = false;
              searchString = input;
              nextResult();
            }
          }
          // enter: next result; shift+enter: previous result
          else {
            if (event.shiftKey) previousResult();
            else nextResult();
          }
        } else if (input !== searchString) {
          searchboxDirty = true;
          clearSearch();
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
    if (!searchElement) createDialog();
    searchElement.style.display = "flex";
    searchInput.focus();
    searchInput.select();
  }

  function closeSearch() {
    if (!searchElement) createDialog();
    searchElement.style.display = "none";
    clearSearch();
  }

  function toggleSearch() {
    if (!searchElement) createDialog();
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
    disableButtons();
    amountSpan.innerText = `0 / 0`;
    amountLabel.innerText = `${l10n.noMatches}`;
  }

  /**
   * Update text of labels when matches were found and enable next and prev buttons.
   */
  function updateLabels(matchIndex) {
    enableButtons();
    amountSpan.innerText = `${matchIndex + 1} / ${matchedSlides.length}`;
    amountLabel.innerText = `${matchIndex + 1}. ${l10n.of} ${
      matchedSlides.length
    } ${l10n.matches}`;
  }

  function disableButtons() {
    searchPrev.setAttribute("aria-disabled", "true");
    searchNext.setAttribute("aria-disabled", "true");
  }

  function enableButtons() {
    searchPrev.removeAttribute("aria-disabled");
    searchNext.removeAttribute("aria-disabled");
  }

  function nextResult() {
    if (matchedSlides && matchedSlides.length > 0) {
      matchIndex = (matchIndex + 1) % matchedSlides.length;
      deck.slide(matchedSlides[matchIndex].h, matchedSlides[matchIndex].v);
      updateLabels(matchIndex);
    } else {
      setLabelToNoMatches();
    }
  }

  function previousResult() {
    if (matchedSlides && matchedSlides.length > 0) {
      matchIndex =
        (matchIndex - 1 + matchedSlides.length) % matchedSlides.length;
      deck.slide(matchedSlides[matchIndex].h, matchedSlides[matchIndex].v);
      updateLabels(matchIndex);
    } else {
      setLabelToNoMatches();
    }
  }

  function clearSearch() {
    CSS.highlights.clear();
    setLabelToNoMatches();
    disableButtons();
    matchedSlides = null;
    matchIndex = -1;
  }

  function doSearch(input) {
    if (!input) return;

    // clear previous search results
    CSS.highlights.clear();
    matchIndex = -1;
    matchedSlides = [];

    // setup regular expression
    const regex = new RegExp(
      "(" +
        input.replace(/^[^\w]+|[^\w]+$/g, "").replace(/[^\w'-]+/g, "|") +
        ")",
      "ig"
    );

    // traverse all text nodes in slides container
    const highlights = new Highlight();
    const slides = deck.getSlidesElement();
    const treeWalker = document.createTreeWalker(slides, NodeFilter.SHOW_TEXT);
    let node = treeWalker.nextNode();
    while (node) {
      const text = node.textContent.toLowerCase();
      if (text) {
        const matches = [...text.matchAll(regex)];
        for (const match of matches) {
          // which slide are we on?
          const slide = node.parentElement.closest("section");
          const slideIndex = deck.getIndices(slide);

          // add slide to matchedSlides array
          let alreadyAdded = false;
          for (const idx of matchedSlides)
            if (idx.h === slideIndex.h && idx.v === slideIndex.v)
              alreadyAdded = true;
          if (!alreadyAdded) matchedSlides.push(slideIndex);

          // add matching range to highlights
          const range = new Range();
          range.setStart(node, match.index);
          range.setEnd(node, match.index + match[0].length);
          highlights.add(range);
        }
      }
      node = treeWalker.nextNode();
    }

    // highlight the found ranges
    CSS.highlights.set("search-plugin-highlight", highlights);
  }

  return {
    id: "search",

    init: (reveal) => {
      deck = reveal;

      deck.registerKeyboardShortcut("CTRL + F", "Search");
      document.addEventListener(
        "keydown",
        function (event) {
          if (event.key == "f" && (event.ctrlKey || event.metaKey)) {
            // If Handout Mode is active do a normal document search
            if (deck.getPlugin("handout")?.isActive()) return;
            event.preventDefault();
            toggleSearch();
          }
        },
        false
      );
    },

    open: openSearch,
    toggle: toggleSearch,
  };
};

export default Plugin;
