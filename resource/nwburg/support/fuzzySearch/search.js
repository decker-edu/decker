import FuzzySet from "./fuzzyset.js";

function setupFuzzySearch(anchor, jsonIndex, minScore = 0.5) {
  // Create search input
  const searchInput = document.createElement("input");
  searchInput.className = "search";
  searchInput.placeholder = "Search materials...";
  anchor.appendChild(searchInput);

  const cards = document.querySelectorAll(".grid-item");
  const fuzzy = FuzzySet(Object.keys(jsonIndex.index)); // Initialize FuzzySet with the index keys

  // Add event listener for input
  searchInput.addEventListener("input", (e) => {
    const query = e.target.value.trim();
    const matches = fuzzy.get(query, [], minScore);

    // Reset all cards
    cards.forEach((card) => card.classList.remove("hidden"));

    if (query && matches) {
      const matchedDeckUrls = new Set(
        matches
          .map((match) => jsonIndex.index[match[1]]) // Get the matching slides
          .flat() // Flatten the array of results
          .map((slide) => jsonIndex.slides[slide.slide]?.deckUrl) // Get deck URLs
      );

      // Hide cards that are not in the matched deck URLs
      cards.forEach((card) => {
        const cardUrl = card.querySelector("a")?.getAttribute("href");
        if (!matchedDeckUrls.has(cardUrl)) {
          card.classList.add("hidden");
        }
      });
    }
  });

  // Reset on Escape key
  searchInput.addEventListener("keyup", (e) => {
    if (e.key === "Escape") {
      searchInput.value = "";
      cards.forEach((card) => card.classList.remove("hidden"));
    }
  });
}

export default setupFuzzySearch;