/*!
 * Handles finding a text string anywhere in the slides and showing the next occurrence to the user
 * by navigatating to that slide and highlighting it.
 *
 * @author Jon Snyder <snyder.jon@gmail.com>, February 2013
 */

const Plugin = () => {
  // The reveal.js instance this plugin is attached to
  let deck;

  let searchElement;
  let searchInput;

  let matchedSlides;
  let currentMatchedIndex;
  let searchboxDirty;
  let hilitor;

  function render() {
    searchElement = document.createElement("div");
    searchElement.classList.add("searchbox");

    // MARIO: adjust position, size, color
    searchElement.style.padding = "calc(var(--icon-size) * 0.5)";
    searchElement.style.borderRadius = "0.25em";
    searchElement.style.background = "white";
    searchElement.style.fontSize = "var(--icon-size)";
    searchElement.style.color = "var(--icon-active-color)";
    searchElement.style.display = "flex";
    searchElement.style.flexDirection = "column";

    // MARIO: adjust border color and search icon (requires font-awesome)
    searchElement.innerHTML = `<div id="searchrow" style="display:flex; align-items:center;">
  <i class="fa-button fas fa-search" style="padding-right: 10px;"></i>
  <input type="search" id="searchinput" placeholder="Search...">
  <span id="searchamount" aria-live="polite">0 / 0</span>
  </div>
  <div id="resultrow">
    <span id="resulttext"></span>
  </div>`;

    // MARIO: override some styling
    searchInput = searchElement.querySelector("#searchinput");
    searchInput.style.fontSize = "1.2rem";
    searchInput.style.width = "10em";
    searchInput.style.padding = "4px 6px";
    searchInput.style.marginRight = "12px";
    searchInput.style.color = "#000";
    searchInput.style.background = "#fff";
    searchInput.style.borderRadius = "2px";
    searchInput.style.border = "2px solid var(--icon-active-color)";
    searchInput.style.outline = "0";
    searchInput.style["-webkit-appearance"] = "none";

    if (!deck.hasPlugin("ui-anchors")) {
      console.error("no decker ui anchor plugin loaded");
    } else {
      deck.getPlugin("ui-anchors").placeButton(searchElement, "TOP_LEFT");
    }

    searchInput.addEventListener(
      "keyup",
      function (event) {
        switch (event.keyCode) {
          case 13:
            event.preventDefault();
            doSearch();
            searchboxDirty = false;
            break;

          // MARIO: close search field on key Escape
          case 27:
            closeSearch();
            break;

          default:
            searchboxDirty = true;
        }
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
  }

  function toggleSearch() {
    if (!searchElement) render();

    if (searchElement.style.display !== "flex") {
      openSearch();
    } else {
      closeSearch();
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
        currentMatchedIndex = 0;
      }
    }

    if (matchedSlides) {
      //navigate to the next slide that has the keyword, wrapping to the first if necessary
      if (matchedSlides.length && matchedSlides.length <= currentMatchedIndex) {
        currentMatchedIndex = 0;
      }
      if (matchedSlides.length > currentMatchedIndex) {
        console.log(matchedSlides[currentMatchedIndex]);
        deck.slide(
          matchedSlides[currentMatchedIndex].h,
          matchedSlides[currentMatchedIndex].v
        );
        const amountSpan = searchElement.querySelector("#searchamount");
        amountSpan.innerText = `${currentMatchedIndex + 1} / ${
          matchedSlides.length
        }`;
        currentMatchedIndex++;
      }
      const nextIndex =
        currentMatchedIndex >= matchedSlides.length ? 0 : currentMatchedIndex;
      const nextSlide = deck.getSlide(
        matchedSlides[nextIndex].h,
        matchedSlides[nextIndex].v
      );
      const nextHeader = nextSlide.querySelector("h1");
      if (nextHeader) {
        const resultSpan = searchElement.querySelector("#resulttext");
        resultSpan.innerText = `Next: ${nextHeader.textContent}`;
      }
    } else {
      const amountSpan = searchElement.querySelector("#searchamount");
      amountSpan.innerText = `0 / 0`;
    }
  }

  // Original JavaScript code by Chirp Internet: www.chirp.com.au
  // Please acknowledge use of this code by including this header.
  // 2/2013 jon: modified regex to display any match, not restricted to word boundaries.
  function Hilitor(selector, tag) {
    var targetNode = document.querySelector(selector) || document.body;
    console.log(targetNode);
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
      return matchRegex
        .toString()
        .replace(/^\/\\b\(|\)\\b\/i$/g, "")
        .replace(/\|/g, " ");
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
          console.log(node);
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
      while (arr.length && (el = arr[0])) {
        el.parentNode.replaceChild(el.firstChild, el);
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
