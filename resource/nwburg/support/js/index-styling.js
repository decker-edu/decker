"use strict";

async function parseChapters() {
    const chapters = [];
    const chapterElements = document.querySelectorAll('#chapters > .chapter');

    for (const chapterEl of chapterElements) {
        const chapterTitle = chapterEl.querySelector('h2')?.textContent?.trim() || 'Unknown Chapter';
        const chapterDate = chapterEl.querySelector('.date')?.textContent?.replace('Date: ', '').trim() || 'Unknown Date';
        const materialsEl = chapterEl.querySelector('.materials');

        const materials = [];
        if (materialsEl) {
            const materialElements = materialsEl.querySelectorAll('.material'); // Each material is now represented as a list item
            for (const materialEl of materialElements) {
                const materialTitle = materialEl.querySelector('.material-title')?.textContent.trim() || 'Untitled';

                // Handle the new `files` array format
                const fileLinks = materialEl.querySelectorAll('a.file-link'); // Detect individual files
                const files = Array.from(fileLinks).map(fileLink => ({
                    Name: fileLink.textContent.trim() || materialTitle,
                    Path: fileLink.getAttribute('href') || '',
                    // if no icon is provided use an html5 default icon for assets <i class="fa-solid fa-paperclip"></i>
                    Icon: fileLink.dataset.icon || '/support/vendor/images/theme/general/hci-logo-red.png',
                    IsReady: true, // Default assumption, update if dynamic readiness info is added
                }));

                // Extract keywords
                const keywordElements = materialEl.querySelectorAll('.keywords .keyword-box');
                const keywords = Array.from(keywordElements).map(kwEl => kwEl.textContent.replace('#', '').trim());

                // Add material
                materials.push({
                    title: materialTitle,
                    files: files.length > 0 ? files : undefined,
                    visible: true,  // Default visibility
                    keywords: keywords,
                });
            }
        }

        chapters.push({
            title: chapterTitle,
            date: chapterDate,
            materials: materials,
        });
    }

    return chapters;
}

/**
 * Fetch and extract metadata from an HTML file.
 * @param {string} url - The URL of the HTML file.
 * @returns {Promise<object>} Metadata extracted from the <head> section.
 */
async function fetchMetadata(url) {
    const response = await fetch(url);
    if (!response.ok) throw new Error(`Failed to fetch ${url}: ${response.statusText}`);

    const text = await response.text();
    const parser = new DOMParser();
    const doc = parser.parseFromString(text, "text/html");

    const meta = {};
    meta.description = doc.querySelector('meta[name="description"]')?.content || "";
    meta.teaserImage = doc.querySelector('meta[name="teaser-image"]')?.content ;
    meta.author = doc.querySelector('meta[name="author"]')?.content || "Unknown Author";
    meta.date = doc.querySelector('meta[name="dcterms.date"]')?.content || "Unknown Date";
    meta.keywords = (doc.querySelector('meta[name="keywords"]')?.content || "")
        .split(",")
        .map(keyword => keyword.trim());

    // Add metadata for multi-file handling
    meta.files = Array.from(doc.querySelectorAll('meta[name="file"]')).map(tag => ({
        Name: tag.getAttribute('content') || "Unnamed File",
        Path: tag.getAttribute('data-path') || "",
        Icon: tag.getAttribute('data-icon') || "default-icon.png",
        IsReady: tag.getAttribute('data-is-ready') === "true",
    }));

    return meta;
}

async function populateCards(chaptersPromise) {
    const chapters = await chaptersPromise;
    const cardGrid = document.getElementById('tileContainer');
    if (!cardGrid) return;

    cardGrid.innerHTML = ''; // Clear any existing content

    for (let chapterIndex = 0; chapterIndex < chapters.length; chapterIndex++) {
        const chapter = chapters[chapterIndex];

        // Create a separator for each chapter
        const separator = document.createElement('div');
        separator.classList.add('chapter-separator');
        separator.dataset.chapterIndex = chapterIndex; // Tag the separator with a chapter index
        separator.innerHTML = `
            <h2>${chapter.title}</h2>
          
        `;
        cardGrid.appendChild(separator);

        // Render cards for the materials in the chapter sequentially
        for (let materialIndex = 0; materialIndex < chapter.materials.length; materialIndex++) {
            const material = chapter.materials[materialIndex];

            const card = document.createElement('div');
            card.classList.add('grid-item');
            card.dataset.chapterIndex = chapterIndex; // Tag the card with the chapter index
            card.dataset.materialIndex = materialIndex; // Optional for debugging or additional functionality

            // Create keyword boxes
            const keywordsHTML = material.keywords.map(keyword => `
                <span class="keyword-box">${keyword}</span>
            `).join('');

            // Create file links for the back side of the card
            const filesHTML = material.files ? material.files.map(file => `
                <a href="${file.Path}" target="_blank" class="file-link">
                    <img src="${file.Icon}" alt="${file.Name}" title="${file.Name}" style="width: 24px; height: 24px;">
                    <span>${file.Name}</span>
                </a>
            `).join('') : '';

            try {
                // Fetch the metadata for the first file
                const meta = await fetchMetadata(material.files[0].Path);

                // Update material object with fetched metadata
                if (!material.description) material.description = meta.description;
                if (!material.teaserImage) material.teaserImage = meta.teaserImage;
                if (!material.author) material.author = meta.author;
                if (!material.date) material.date = meta.date;
                if (material.keywords.length === 0) material.keywords = meta.keywords;

                card.innerHTML = `
                        <div class="card">
                        <div class="image-container">
                                <img src="${material.teaserImage || 'preview-image.webp'}">
                            </div>
                            <div class="description-container">
                                <a class="card-title">${material.title}</a>
                                <p>${material.description || 'No description available'}</p>
                                <div class="keywords">${keywordsHTML}</div>
                            </div>
                            <div class="card-footer-container">
                                <p class="card-date">${chapter.date}</p>
                                <p class="card-more-btn"> ${'More'}</p>
                            </div>
                        </div>`
                        ;

                // Add identifier based on the URLs of the material files to be used for filtering
                if (material.files) {
                    card.dataset.urls = material.files.map(file => file.Path).join(',');
                } else {
                    card.dataset.url = material.file;

                }

                cardGrid.appendChild(card);

            } catch (error) {
                console.error("Error fetching metadata:", error);
            }
        }
    }

    // Update separator visibility based on card visibility
    updateSeparators();
}

/**
 * Updates the visibility of chapter separators based on the visibility of their associated cards.
 */
function updateSeparators() {
    const separators = document.querySelectorAll('.chapter-separator');
    separators.forEach(separator => {
        const chapterIndex = separator.dataset.chapterIndex;
        const cards = document.querySelectorAll(`.grid-item[data-chapter-index="${chapterIndex}"]`);
        const hasVisibleCards = Array.from(cards).some(card => card.offsetParent !== null); // Check visibility
        separator.style.display = hasVisibleCards ? 'block' : 'none';
    });
}

document.addEventListener("DOMContentLoaded", async function () {
    try {
        const chapters = await parseChapters();
        await populateCards(chapters);
    } catch (error) {
        console.error("Error in main execution:", error);
    }
});