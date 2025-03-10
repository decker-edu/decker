// search-functionality.js
"use strict";

import FuzzySet from "../fuzzySearch/fuzzyset.js";

document.addEventListener("DOMContentLoaded", () => {
  const anchor = document.getElementById("search-anchor");

  //debug statement
  console.log("search-anchor: ", anchor);

  // Add search input to the anchor
  anchor.innerHTML = `
    <p>
      <i class="fa-solid fa-magnifying-glass"></i>
      <input class="search" placeholder="Search in slides" type="text">
    </p>
  `;

  const searchInput = anchor.querySelector("input.search");

  // Fetch index.json
  fetch("./index.json")
    .then((response) => response.json())
    .then((indexData) => {
      // Initialize fuzzy search logic
      const fuzzy = FuzzySet(Object.keys(indexData.index));

      // Attach search event listener
      searchInput.addEventListener("input", (event) => {
        const query = event.target.value.trim();

        if (!query) {
          // Reset filters when the search bar is empty
          resetFilters();
          return;
        }

        // Perform fuzzy search
        const matches = fuzzy.get(query, [], 0.6);

        if (matches && matches.length > 0) {
          // Extract slide URLs from matches
          const matchingSlides = matches.flatMap((match) =>
            indexData.index[match[1]].map((item) => item.slide)
          );

          //debug statement
          console.log("matchingSlides: ", matchingSlides);

          // Filter cards based on matching slide URLs
          filterCards(matchingSlides);
        } else {
          // Hide all cards if no matches found
          hideAllCards();
        }
      });
    })
    .catch((error) => console.error("Error loading index.json:", error));
});

// Function to filter cards based on slide URLs. Sets the display property of the parent card element
function filterCards(slideUrls) {
  const cards = document.querySelectorAll(".card");

  cards.forEach((card) => {
    // Assume cards have a `data-url or data-urls` attribute
    const cardUrl = card.parentElement.dataset.url || card.parentElement.dataset.urls; 
    // if the slideUrls array has an element with the prefix of the cardUrl, then display the card
    if (slideUrls.some((url) => card && cardUrl && url.startsWith(cardUrl))) {
      card.parentElement.style.display = "block";
    } else {
      card.parentElement.style.display = "none";
    }
  });

    // Update separator visibility
    updateSeparators();
}

// Function to reset filters
function resetFilters() {
  const cards = document.querySelectorAll(".card");
  cards.forEach((card) => {
    card.parentElement.style.display = "block";
  });
    // Update separator visibility
    updateSeparators();
}

// Function to hide all cards
function hideAllCards() {
  const cards = document.querySelectorAll(".card");
  cards.forEach((card) => {
    card.parentElement.style.display = "none";
  });
    // Update separator visibility
    updateSeparators();
}

function updateSeparators() {
  const separators = document.querySelectorAll('.chapter-separator');

  separators.forEach((separator) => {
    const chapterIndex = separator.dataset.chapterIndex;
    const cards = document.querySelectorAll(`.grid-item[data-chapter-index="${chapterIndex}"]`);
    const hasVisibleCards = Array.from(cards).some((card) => card.style.display !== 'none');

    separator.style.display = hasVisibleCards ? 'block' : 'none';
  });
}