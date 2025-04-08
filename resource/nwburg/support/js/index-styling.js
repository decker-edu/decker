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
                const materialDate = materialEl.querySelector('.date')?.textContent.replace('Date: ', '').trim() || 'Unknown Date';
                const materialImg = materialEl.querySelector('img')?.src || '/support/vendor/images/theme/general/hci-logo-red.png';
                const materialDescription = materialEl.querySelector('.description')?.textContent.trim() || 'No description available';
                const slides = materialEl.querySelector('a.slides-link')?.getAttribute('href') || '';

                const fileLinks = materialEl.querySelectorAll('a.file-link');
                const files = Array.from(fileLinks).map(fileLink => ({
                    name: fileLink.textContent.trim() || 'Resource',
                    link: fileLink.getAttribute('href') || '',
                }));

                const keywordElements = materialEl.querySelectorAll('.keywords .keyword-box');
                const keywords = Array.from(keywordElements).map(kwEl => kwEl.textContent.replace('#', '').trim());

                materials.push({
                    title: materialTitle,
                    description: materialDescription,
                    slides: slides,
                    date: materialDate,
                    teaserImage: materialImg,
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
        name: tag.getAttribute('content') || "Unnamed File",
        link: tag.getAttribute('data-path') || "",
        img: tag.getAttribute('data-icon') || "default-icon.png",
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
            var links = "";
            if (material.files) {
                material.files.forEach((file, index) => {
                    links += (`<a href="${file.link}" class="lecture-resource">${file.name}</a>`);
                });
            }
        
            const lectureCard = document.createElement('div');
            lectureCard.classList.add('lecture-card');
            lectureCard.dataset.chapterIndex = chapterIndex;
            lectureCard.dataset.materialIndex = materialIndex;

            try {
                const meta = await fetchMetadata(material.slides);

                if (!material.description) material.description = meta.description;
                if (!material.teaserImage) material.teaserImage = meta.teaserImage;
                if (!material.author) material.author = meta.author;
                if (!material.date) material.date = meta.date;
                if (material.keywords.length === 0) material.keywords = meta.keywords;

                lectureCard.innerHTML = `
                    <div class="lecture-img-container">
                        <a class="lecture-more-btn" href=${material.slides}>
                            <img src="${material.teaserImage || 'preview-image.webp'}" alt="${material.title}" />
                        </a>
                    </div>
                    <div class="lecture-title-container">
                        <div class="lecture-title-header">
                            <p class="lecture-title">${material.title}</p>
                            <div class="resource-links">
                                ${links}
                            </div>
                        </div>
                        <div class="lecture-description">${material.description || 'No description available'}</div>
                        <span class="tooltip">${material.description}</span>
                    </div>
                    <div class="lecture-footer">
                        <p class="lecture-date">${material.date}</p>
                        <a class="lecture-more-btn" href=${material.slides}>
                            <svg width="20px" height="20px" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
                                <path d="M10 7L15 12L10 17" stroke="gray" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round"/>
                            </svg>
                            <p>More</p>
                        </a>
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
        const titleHeaders = document.querySelectorAll(".lecture-title-container"); 

        lectureHeaders.forEach(lh => {
            lh.addEventListener("click", function () {
                const lectureContainer = this.nextElementSibling; 
                
                if (lectureContainer) {
                    lectureContainer.style.display = (lectureContainer.style.display === "none" || lectureContainer.style.display === "") ? "block" : "none";
                }
            });
        });
        titleHeaders.forEach(th => {
            th.addEventListener("mouseenter", function () {
                const tooltip = th.querySelector(".tooltip");
                if(tooltip) {
                    tooltip.style.visibility = "visible";
                    tooltip.style.opacity = "1";
                }
            });
            th.addEventListener("mouseleave", function () {
                const tooltip = th.querySelector(".tooltip");
                if(tooltip) {
                    tooltip.style.visibility = "hidden";
                    tooltip.style.opacity = "";
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
