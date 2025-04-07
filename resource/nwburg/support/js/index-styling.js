"use strict";

// Fetching and parsing chapters
async function parseChapters() {
    const chapters = [];
    const chapterElements = document.querySelectorAll('#chapters > .chapter');

    for (const chapterEl of chapterElements) {
        const chapterTitle = chapterEl.querySelector('h2')?.textContent?.trim() || 'Unknown Chapter';
        const chapterDate = chapterEl.querySelector('.date')?.textContent?.replace('Date: ', '').trim() || 'Unknown Date';
        const materialsEl = chapterEl.querySelector('.materials');

        const materials = [];
        if (materialsEl) {
            const materialElements = materialsEl.querySelectorAll('.material');
            for (const materialEl of materialElements) {
                const materialTitle = materialEl.querySelector('.material-title')?.textContent.trim() || 'Untitled';

                const fileLinks = materialEl.querySelectorAll('a.file-link');
                const files = Array.from(fileLinks).map(fileLink => ({
                    Name: fileLink.textContent.trim() || materialTitle,
                    Path: fileLink.getAttribute('href') || '',
                    Icon: fileLink.dataset.icon || '/support/vendor/images/theme/general/hci-logo-red.png',
                    IsReady: true,
                }));

                const keywordElements = materialEl.querySelectorAll('.keywords .keyword-box');
                const keywords = Array.from(keywordElements).map(kwEl => kwEl.textContent.replace('#', '').trim());

                materials.push({
                    title: materialTitle,
                    files: files.length > 0 ? files : undefined,
                    visible: true,
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

// Fetch metadata from a file
async function fetchMetadata(url) {
    const response = await fetch(url);
    if (!response.ok) throw new Error(`Failed to fetch ${url}: ${response.statusText}`);

    const text = await response.text();
    const parser = new DOMParser();
    const doc = parser.parseFromString(text, "text/html");

    const meta = {};
    meta.description = doc.querySelector('meta[name="description"]')?.content || "";
    meta.teaserImage = doc.querySelector('meta[name="teaser-image"]')?.content;
    meta.author = doc.querySelector('meta[name="author"]')?.content || "Unknown Author";
    meta.date = doc.querySelector('meta[name="dcterms.date"]')?.content || "Unknown Date";
    meta.keywords = (doc.querySelector('meta[name="keywords"]')?.content || "")
        .split(",")
        .map(keyword => keyword.trim());

    meta.files = Array.from(doc.querySelectorAll('meta[name="file"]')).map(tag => ({
        Name: tag.getAttribute('content') || "Unnamed File",
        Path: tag.getAttribute('data-path') || "",
        Icon: tag.getAttribute('data-icon') || "default-icon.png",
        IsReady: tag.getAttribute('data-is-ready') === "true",
    }));

    return meta;
}

// Populate the cards (lecture materials)
async function populateCards(chaptersPromise) {
    const chapters = await chaptersPromise;
    const cardGrid = document.getElementById('tileContainer');
    if (!cardGrid) return;

    cardGrid.innerHTML = ''; // Clear existing content

    for (let chapterIndex = 0; chapterIndex < chapters.length; chapterIndex++) {
        const chapter = chapters[chapterIndex];

        const chapterHeader = document.createElement('div');
        chapterHeader.classList.add('lecture-header');
        chapterHeader.dataset.chapterIndex = chapterIndex;
        chapterHeader.innerHTML = `<h2>${chapter.title}</h2>`;
        cardGrid.appendChild(chapterHeader);

        const lectureContainer = document.createElement('div');
        lectureContainer.classList.add('lecture-container');

        const lectureCardsWrapper = document.createElement('div');
        lectureCardsWrapper.classList.add('lecture-cards-wrapper');

        for (let materialIndex = 0; materialIndex < chapter.materials.length; materialIndex++) {
            const material = chapter.materials[materialIndex];

            const lectureCard = document.createElement('div');
            lectureCard.classList.add('lecture-card');
            lectureCard.dataset.chapterIndex = chapterIndex;
            lectureCard.dataset.materialIndex = materialIndex;

            try {
                const meta = await fetchMetadata(material.files[0].Path);

                if (!material.description) material.description = meta.description;
                if (!material.teaserImage) material.teaserImage = meta.teaserImage;
                if (!material.author) material.author = meta.author;
                if (!material.date) material.date = meta.date;
                if (material.keywords.length === 0) material.keywords = meta.keywords;

                lectureCard.innerHTML = `
                <div class="lecture-card-box">
                    <div class="lecture-img-container">
                        <img src="${material.teaserImage || 'preview-image.webp'}">
                    </div>
                    <div class="lecture-description">
                        <a class="lecture-title">${material.title}</a>
                        <p>${material.description || 'No description available'}</p>
                        <span class="tooltip">${material.description}</span>
                    </div>
                    <div class="lecture-footer">
                        <p class="lecture-date">${chapter.date}</p>
                        <p class="lecture-more-btn">
                            <svg width="20px" height="20px" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
                                <path d="M10 7L15 12L10 17" stroke="gray" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"/>
                            </svg> More
                        </p>
                    </div>
                </div>`;

                lectureCardsWrapper.appendChild(lectureCard);
            } catch (error) {
                console.error("Error fetching metadata:", error);
            }
        }

        lectureContainer.appendChild(lectureCardsWrapper);
        cardGrid.appendChild(lectureContainer);
    }

    updateSeparators();
}

// Update the visibility of chapter separators based on content visibility
function updateSeparators() {
    const separators = document.querySelectorAll('.chapter-separator');
    separators.forEach(separator => {
        const chapterIndex = separator.dataset.chapterIndex;
        const cards = document.querySelectorAll(`.grid-item[data-chapter-index="${chapterIndex}"]`);
        const hasVisibleCards = Array.from(cards).some(card => card.offsetParent !== null);
        separator.style.display = hasVisibleCards ? 'block' : 'none';
    });
}

// Toggle lecture content visibility on title click
document.addEventListener("DOMContentLoaded", async function () {
    try {
        const chapters = await parseChapters();
        await populateCards(chapters);

        // Add event listeners for toggling lecture content visibility
        const lectureHeaders = document.querySelectorAll(".lecture-header");

        lectureHeaders.forEach(header => {
            header.addEventListener("click", function () {
                const lectureContainer = this.nextElementSibling; // The lecture content section
                
                if (lectureContainer) {
                    // Toggle visibility of content
                    lectureContainer.style.display = (lectureContainer.style.display === "none" || lectureContainer.style.display === "") ? "block" : "none";
                }
            });
        });

        // Initially hide all lecture content sections
        document.querySelectorAll(".lecture-container").forEach(container => {
            container.style.display = "block";
        });
    } catch (error) {
        console.error("Error in main execution:", error);
    }
});
