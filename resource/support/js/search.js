export default setupSearch;

import FuzzySet from "./fuzzyset.js";

function setupSearch(
  anchor,
  minScore = 0.5,
  showDeckTitles = true,
  showDeckSubtitles = false
) {
  fetch("./index.json")
    .then((res) => res.json())
    .then((index) => {
      setup(index, anchor, minScore, showDeckTitles, showDeckSubtitles);
    })
    .catch((err) => console.log("cannot load: index.json", err));
}

function setup(index, anchor, minScore, showDeckTitles, showDeckSubtitles) {
  anchor.innerHTML = `
        <p>
            <input class="search" placeholder="Looking for something?" type="text">
        </p>
        <p>
            <table class="search">
                <thead><tr><th>Word</th><th>Deck</th><th>Slide</th><th>Hits</th></tr></thead>
                <tbody></tbody>
            </table>
        </p>
    `;

  const search = anchor.querySelector("input.search");
  const table = anchor.querySelector("table.search");
  const results = anchor.querySelector("table.search tbody");
  const keys = Object.keys(index.index).map((k) => k.toString());
  const fuzzy = FuzzySet(keys);

  search.addEventListener("keyup", (e) => {
    // delete all rows in table body
    while (results.firstChild) {
      results.removeChild(results.firstChild);
    }

    // get matches from fuzzy set
    const matches = fuzzy.get(search.value, [], minScore);

    // create one table row per slide per match
    for (let match of matches) {
      const word = match[1];
      const exact = search.value == word;
      const found = exact ? `<b>${word}</b>` : `<i>${word}</i>`;
      let onSlides = index.index[word];

      // sort slide by search count
      onSlides.sort((a, b) => b.count - a.count);

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
        item.innerHTML = `<td>${found}</td> 
        <td><a target="_blank" href="./${dInfo.deckUrl}">${deck}</a></td>
        <td><a target="_blank" href="./${url}">${sInfo.slideTitle}</a></td>
        <td>${count}</td>`;

        results.appendChild(item);
      }
    }
  });
}
