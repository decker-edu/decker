export default setupSearch;

import FuzzySet from "./fuzzyset.js";

const l10n_de = {
  details_summary: "In den Folien suchen",
  searchbar_label: "Suchbegriff eingeben",
  searchresults: "Suchergebnisse",
  noentry: "Keine Eingabe",
  noresults: "Keine Resultate",
  results: "Resultate",
  thword: "Begriff",
  thdeck: "Foliensatz",
  thslide: "Folie",
  thcount: "Treffer",
  unknownDeck: "Unbekannter Foliensatztitel",
  unknownSlide: "Unbekannter Folientitel",
};

const l10n_en = {
  details_summary: "Search in slides",
  searchbar_label: "Enter search term",
  searchresults: "Search results",
  noentry: "No entry",
  noresults: "No results",
  results: "results",
  thword: "Term",
  thdeck: "Deck",
  thslide: "Slide",
  thcount: "Hits",
  unknownDeck: "Unknown deck title",
  unknownSlide: "Unknown slide title",
};

const l10n = navigator.language === "de" ? l10n_de : l10n_en;

function setupSearch(
  anchor,
  minScore = 0.5,
  showDeckTitles = true,
  showDeckSubtitles = false
) {
  let indexPath = Decker.meta.projectPath;
  if (!indexPath.endsWith("/")) indexPath += "/";
  indexPath += "index.json";
  console.log("read search index from " + indexPath);

  fetch(indexPath)
    .then((res) => {
      if (res.ok) return res.json();
      else throw new Error("Cannot download index file.");
    })
    .then((index) => {
      setup(index, anchor, minScore, showDeckTitles, showDeckSubtitles);
    })
    .catch((err) => console.log(err));
}

function setup(index, anchor, minScore, showDeckTitles, showDeckSubtitles) {
  if (anchor.innerHTML.trim() === "") {
    anchor.innerHTML = `<details role="search">
  <summary>${l10n.details_summary}</summary>
  <p><input class="search" placeholder="${l10n.searchbar_label}" type="search"></p>
  <h2 id="searchresultheader">
    ${l10n.searchresults}: ${l10n.noentry}
  </h2>
  <table class="search">
    <thead><tr><th>${l10n.thword}</th><th>${l10n.thdeck}</th><th>${l10n.thslide}</th><th>${l10n.thcount}</th></tr></thead>
    <tbody></tbody>
  </table>
</details>`;
  }
  const search = anchor.querySelector("input.search");
  const table = anchor.querySelector("table.search");
  const header = anchor.querySelector("#searchresultheader");
  const results = anchor.querySelector("table.search tbody");
  const keys = Object.keys(index.index).map((k) => k.toString());
  const fuzzy = FuzzySet(keys);

  let selectedRow = null;

  table.addEventListener("focusin", (event) => {
    const target = event.target;
    if (target.tagName === "TR") {
      selectedRow = target;
    }
  });

  /* Remove tabindex attribute of all links in row */
  function enableLinks(row) {
    const links = row.querySelectorAll("a");
    for (const link of links) {
      link.removeAttribute("tabindex");
    }
    if (links.length > 0) {
      links[0].focus();
    }
    row.setAttribute("tabindex", -1);
  }

  /* Set tabindex to -1 to all links in row */
  function disableLinks(row) {
    const links = row.querySelectorAll("a");
    for (const link of links) {
      link.setAttribute("tabindex", -1);
    }
    row.setAttribute("tabindex", 0);
  }

  /* Keyboard navigation through the result list */
  table.addEventListener("keydown", (event) => {
    if (!selectedRow) {
      return;
    }
    if (event.key === "ArrowDown") {
      if (selectedRow) {
        const next = selectedRow.nextElementSibling;
        if (next) {
          disableLinks(selectedRow);
          selectedRow.setAttribute("tabindex", -1);
          selectedRow = next;
          next.focus();
          event.preventDefault();
        }
      }
    }
    if (event.key === "ArrowUp") {
      if (selectedRow) {
        const prev = selectedRow.previousElementSibling;
        if (prev) {
          disableLinks(selectedRow);
          selectedRow.setAttribute("tabindex", -1);
          selectedRow = prev;
          prev.focus();
          event.preventDefault();
        }
      }
    }
    if (event.key === "Enter") {
      if (document.activeElement === selectedRow) {
        enableLinks(selectedRow);
        selectedRow.setAttribute("tabindex", -1);
        event.preventDefault();
      }
    }
  });

  /* When the focus moves out of the table, disable current row */
  table.addEventListener("focusout", (event) => {
    if (table.contains(event.relatedTarget)) {
      return;
    }
    if (selectedRow) {
      disableLinks(selectedRow);
    }
  });

  /* When enter is pressed on searchbar, move focus to results header */
  search.addEventListener("keydown", (event) => {
    if (event.key === "Enter") {
      header.setAttribute("tabindex", 0);
      header.focus();
    }
  });

  /* When the header gets unfocused, remove tabindex */
  header.addEventListener("blur", (event) => {
    header.removeAttribute("tabindex");
  });

  /* Update results table on searchbar input change */
  search.addEventListener("input", (e) => {
    // delete all rows in table body
    while (results.firstChild) {
      results.removeChild(results.firstChild);
      selectedRow = null;
    }

    // get matches from fuzzy set
    const matches = fuzzy.get(search.value, [], minScore);
    anchor.setAttribute("data-results", matches.length);
    if (matches.length) {
      anchor.classList.add("results");
      header.innerText = `${l10n.searchresults}: ${matches.length} ${l10n.results}`;
    } else {
      if (search.value === "") {
        header.innerText = `${l10n.searchresults}: ${l10n.noentry}`;
      } else {
        header.innerText = `${l10n.searchresults}: ${l10n.noresults}`;
      }
      anchor.classList.remove("results");
    }

    // create one table row per slide per match
    for (let match of matches) {
      const word = match[1];
      const exact = search.value == word;
      const found = exact ? `<b>${word}</b>` : `<i>${word}</i>`;
      let onSlides = index.index[word];

      // first: sort based on deck URL
      onSlides.sort((slide1, slide2) => {
        const deck1 = index.slides[slide1.slide].deckUrl;
        const deck2 = index.slides[slide2.slide].deckUrl;
        if (deck1 < deck2) return -1;
        if (deck1 > deck2) return 1;
        return 0;
      });

      // second: (stable) sort based on search count
      onSlides.sort((slide1, slide2) => slide2.count - slide1.count);

      for (let slide of onSlides) {
        const url = slide.slide;
        const count = slide.count;
        const sInfo = index.slides[url];
        const dInfo = index.decks[sInfo.deckUrl];

        let deck = "";
        if (showDeckTitles && dInfo.deckTitle) {
          deck += dInfo.deckTitle;
        }
        if (showDeckSubtitles && dInfo.deckSubtitle) {
          if (deck.length) deck += " &mdash; ";
          deck += dInfo.deckSubtitle;
        }

        let item = document.createElement("tr");
        item.ariaLabel = `${l10n.thword}: ${word}, ${l10n.thdeck}: ${
          deck !== "" ? deck : l10n.unknownDeck
        }, ${l10n.thslide}: ${
          sInfo.slideTitle !== "" ? sInfo.slideTitle : l10n.unknownSlide
        }, ${l10n.thcount}: ${count}`;
        item.innerHTML = `<td>${found}</td> 
        <td><a target="_blank" tabindex="-1" href="./${dInfo.deckUrl}">${
          deck !== "" ? deck : l10n.unknownDeck
        }</a></td>
        <td><a target="_blank" tabindex="-1" href="./${url}">${
          sInfo.slideTitle !== "" ? sInfo.slideTitle : l10n.unknownSlide
        }</a></td>
        <td>${count}</td>`;

        item.setAttribute("tabindex", -1);

        const indexMode = Decker.meta.index?.mode || "insert";
        if (indexMode === "modal") {
          const links = item.querySelectorAll("a");
          for (const link of links) {
            addModalToLink(link);
          }
        }

        results.appendChild(item);
      }
      if (results.firstElementChild) {
        results.firstElementChild.setAttribute("tabindex", 0);
      }
    }
  });

  search.addEventListener("keyup", (e) => {
    if (e.key == "Escape") {
      search.value = "";
      while (results.firstChild) {
        results.removeChild(results.firstChild);
      }
      anchor.classList.remove("results");
    }
  });

  // if search elements are inside a <details> element, add some
  // convenience functionality
  const details = search.closest("details");
  if (details) {
    details.addEventListener("toggle", (evt) => {
      if (details.open) search.focus();
    });

    search.addEventListener("keyup", (e) => {
      if (e.key == "Escape") details.open = false;
    });
  }
}
