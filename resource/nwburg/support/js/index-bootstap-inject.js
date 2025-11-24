"use strict";
async function extractPresentations() {
    const presentations = [];
    const sectionHeaders = document.querySelectorAll('h2 + ul');
    // NodeListOf has no iterator. Convert to array to use for of loop
    const sectionHeadersArray = Array.from(sectionHeaders);
    for (const header of sectionHeadersArray) {
        const sectionTitle = header.previousElementSibling?.textContent || '';
        const presentationLinks = header.querySelectorAll('a');
        const presentationLinksArray = Array.from(presentationLinks);
        const presentationsInSection = [];

        //debugging 
//        console.log('sectionTitle:', sectionTitle);
//        console.log('presentationLinksArray:', presentationLinksArray);


        for (const link of presentationLinksArray) {
            

            const presentationTitle = link.textContent;
            const presentationURL = link.getAttribute('href');
            if (!presentationURL) {
                continue;
                // Skip this iteration if presentationURL is null
            }
            // Check if presentationURL is a valid URL
            try {
                // the url is relative to the current page
                new URL(presentationURL, window.location.href);
            }
            catch (error) {
                console.error(`Invalid URL: ${presentationURL}`);
                continue; // Skip this iteration if URL is invalid
            }
            const response = await fetch(presentationURL);
            const presentationHTML = await response.text();
            const parser = new DOMParser();
            const presentationDoc = parser.parseFromString(presentationHTML, 'text/html');
            // Extract chapter and section from the filename. 
            // Removing the path before.
            const filename = presentationURL.split('/').pop();
            if (!filename) {
                continue; // Skip this iteration if filename is null
            }
            const filenameParts = filename.split('-');
            //the chapter, section, subsection and subsubsection are meta data in the header of the presentation
            const chapter = presentationDoc.querySelector('meta[name="chapter"]')?.getAttribute('content') || '';
            const section = presentationDoc.querySelector('meta[name="section"]')?.getAttribute('content') || '';
            const subsection = presentationDoc.querySelector('meta[name="subsection"]')?.getAttribute('content') || '';
            const subsubsection = presentationDoc.querySelector('meta[name="subsubsection"]')?.getAttribute('content') || '';

            const titleElement = presentationDoc.querySelector('.title') || presentationDoc.querySelector('title');
            const subtitleElement = presentationDoc.querySelector('.subtitle');
            const dateElement = presentationDoc.querySelector('.date');
            const authorElement = presentationDoc.querySelector('.author');
            const teaserImageElement = presentationDoc.querySelector('meta[name="teaser-image"]');
            const affiliationLogoElement = presentationDoc.querySelector('.affiliation-logo img');
            const affiliationElement = presentationDoc.querySelector('.affiliation');
            const keywordsElement = presentationDoc.querySelector('meta[name="keywords"]');
            const keywords = keywordsElement ? keywordsElement.getAttribute('content')?.split(', ') : [];
            const visibilityElement = presentationDoc.querySelector('meta[name="visible"]');
            const descriptionElement = presentationDoc.querySelector('meta[name="description"]');

            //             $if(index.supplimentals)$
            //   $for(index.supplimentals)$
            //     <meta name="supplimental" content="title: $index.supplimentals.title$, link: $index.supplimentals.link$, icon: $index.supplimentals.icon$">
            //   $endfor$
            // $endif$
            const supplimentals = presentationDoc.querySelectorAll('meta[name="supplimental"]');
            //           parse the content of the meta tag, split it by comma and then by colon
            const supplimentalsArray = Array.from(supplimentals).map(supplimental => {
                const content = supplimental.getAttribute('content') || '';
                const parts = content.split(',').map(part => part.split(': ').map(part => part.trim()));
                return parts.reduce((acc, [key, value]) => {
                    acc[key] = value;
                    return acc;
                }, {});
            });
            //console.log(supplimentalsArray);





            // Check if the presentation is a handout. If it is, this is a supplemental presentation. Instead of adding it to the presentations array, add it to the supplemental array of the corresponding presentation.

            if (presentationURL.endsWith('-handout.html')) {
                const deckPresentationURL = presentationURL.replace('-handout.html', '-deck.html');
                const correspondingSection = presentations.find(section => section.sectionTitle === sectionTitle);
                if (correspondingSection) {
                    const correspondingPresentation = correspondingSection.sectionPresentations.find(presentation => presentation.url === deckPresentationURL);
                    if (correspondingPresentation) {
                        correspondingPresentation.supplemental.push({
                            title: presentationTitle,
                            url: presentationURL,
                            affiliationLogo: affiliationLogoElement ? affiliationLogoElement.getAttribute('src') || '' : '',
                            subtitle: subtitleElement ? subtitleElement.textContent?.trim() || '' : '',
                            date: dateElement ? dateElement.textContent?.trim() || '' : '',
                        });
                    }
                    else
                        console.error(`Deck presentation not found for handout: ${presentationURL}`);
                }
                continue;
            }
            const presentationInfo = {
                title: titleElement ? titleElement.textContent?.trim() || '' : '',
                subtitle: subtitleElement ? subtitleElement.textContent?.trim() || '' : '',
                date: dateElement ? dateElement.textContent?.trim() || '' : '',
                author: authorElement ? authorElement.textContent?.trim() || '' : '',
                teaserImage: teaserImageElement ? teaserImageElement.getAttribute('content') || '' : '',
                affiliationLogo: affiliationLogoElement ? affiliationLogoElement.getAttribute('src') || '' : '',
                affiliation: affiliationElement ? affiliationElement.textContent?.trim() || '' : '',
                chapter: chapter,
                section: section,
                subsection: subsection,
                subsubsection: subsubsection,
                keywords: keywords || [],
                url: presentationURL,
                supplemental: [],
                description: descriptionElement ? descriptionElement.getAttribute('content') || '' : '',
                visibility: visibilityElement ? visibilityElement.getAttribute('content') || false : false,
            };

            // if there are supplimentals, add them to the presentationInfo
            for (const supplimental of supplimentalsArray) {
                presentationInfo.supplemental.push({
                    title: supplimental.title,
                    url: supplimental.link,
                    affiliationLogo: supplimental.icon,
                    subtitle: "",
                    date: "",
                });
            }

            // const titleParts = filenameParts.slice(section ? 2 : 1);
            // const presentationTitleWithoutOrdinal = titleParts.map((part) => part.charAt(0).toUpperCase() + part.slice(1)).join(' ');
            // if (keywords && keywords.includes('supplemental')) {
            //     // Iterate over all presentationsInSection and each sectionPresentation of them 
            //     // to find the corresponding presentation
            //     let found = false;
            //     for (const sectionPresentation of presentations) {
            //         if (found)
            //             break;
            //         for (const presentation of sectionPresentation.sectionPresentations) {
            //             if (found)
            //                 break;
            //             if (presentation.chapter === chapter) {
            //                 // Query the icon using <link rel="shortcut icon">
            //                 const iconElement = presentationDoc.querySelector('link[rel="shortcut icon"]');
            //                 // Misuse the affiliationLogo to store the icon
            //                 presentationInfo.affiliationLogo = iconElement ? iconElement.getAttribute('href') || '' : '';
            //                 presentation.supplemental.push(presentationInfo);
            //                 found = true;
            //                 break;
            //             }
            //         }
            //     }
            //     if (found) {
            //         // If there is a corresponding presentation,
            //         // add the supplemental presentation (now just an icon and url)
            //         ////
            //         const correspondingPresentation = presentations.find(section => section.sectionTitle === sectionTitle)?.sectionPresentations
            //             .find(presentation => presentation.chapter === chapter);
            //         if (correspondingPresentation) {
            //             correspondingPresentation.supplemental.push({
            //                 title: presentationTitleWithoutOrdinal,
            //                 url: presentationURL,
            //                 affiliationLogo: presentationInfo.affiliationLogo,
            //                 subtitle: "",
            //                 date: "",
            //                 author: "",
            //                 teaserImage: "",
            //                 affiliation: "",
            //                 chapter: "",
            //                 section: "",
            //                 keywords: [],
            //                 supplemental: []
            //             });
            //         }
            //         ////
            //     }
            // }

            
            presentationsInSection.push(presentationInfo);
        }
        presentations.push({
            sectionTitle: sectionTitle,
            sectionPresentations: presentationsInSection,
        });

        //debug
        // console.log('sectionTitle:', sectionTitle);
        // console.log('presentationsInSection:', presentations
        //     .find(section => section.sectionTitle === sectionTitle)?.sectionPresentations);


    }
    removeExcludedDirectories('', presentations);
    removeEmptySections(presentations);
    return presentations;
}

// Deprecated
function createChapterFilterButtons() {
    const chapterTitles = document.querySelectorAll('.chapter-title');
    const filterColumn = document.querySelectorAll('.filter-column .card-title');
    let possibleChapterTile = null;
    //    for (const title of filterColumn) {
    filterColumn.forEach(title => {
        if (title.textContent === 'Chapter') {
            possibleChapterTile = title;
            return;
        }
    });
    if (!possibleChapterTile) {
        return;
    }
    // ChapterTitel can be assumed to be not null
    const chapterTile = possibleChapterTile;
    const grid = document.createElement('div');
    grid.classList.add('grid');
    chapterTile.parentElement?.appendChild(grid);
    //for (const chapterTitle of chapterTitles) {
    chapterTitles.forEach(chapterTitle => {
        const button = document.createElement('button');
        button.classList.add('googlebutton', 'button', 'js-button-filter', 'chapter-button');
        button.setAttribute('data-category', chapterTitle.textContent || '');
        button.textContent = chapterTitle.textContent || '';
        grid.appendChild(button);
    });
}
// Deprecated
function createTagFilterButtons() {
    const keywordElements = document.querySelectorAll('.keyword');
    const grid = document.createElement('div');
    grid.classList.add('grid');
    const filterColumn = document.querySelectorAll('.filter-column .card-title');
    let possibleTagTile = null;
    //for (const title of filterColumn) {
    filterColumn.forEach(title => {
        if (title.textContent === 'Tags') {
            possibleTagTile = title;
            return;
        }
    });
    if (!possibleTagTile) {
        return;
    }
    const tagTile = possibleTagTile;
    tagTile.parentElement?.appendChild(grid);
    keywordElements.forEach(keywordElement => {
        const button = document.createElement('button');
        button.classList.add('googlebutton', 'button', 'js-button-filter', 'keyword-button');
        button.setAttribute('data-category', keywordElement.textContent || '');
        button.textContent = keywordElement.textContent || '';
        grid.appendChild(button);
    });
}
function removeExcludedDirectories(prefix, sections) {
    const excludeDirs = document.getElementById('search-exclude-dirs');
    if (!excludeDirs) {
        return;
    }
    const excludedDirs = Array.from(excludeDirs.children).map(li => li.textContent?.trim() || '');
    for (let i = sections.length - 1; i >= 0; i--) {
        const section = sections[i];
        for (let j = section.sectionPresentations.length - 1; j >= 0; j--) {
            const presentation = section.sectionPresentations[j];
            if (excludedDirs.some(excludedDir => presentation.url.startsWith(prefix + excludedDir))) {
                section.sectionPresentations.splice(j, 1);
            }
        }
    }
}
function removeEmptySections(presentations) {
    for (let i = presentations.length - 1; i >= 0; i--) {
        const section = presentations[i];
        if (section.sectionPresentations.length === 0) {
            presentations.splice(i, 1);
        }
    }
}

function formatTitle(filename) {
    const parts = filename.replace('.html', '').split('-').slice(2, -1);
    return parts.map(part => part.charAt(0).toUpperCase() + part.slice(1)).join(' ');
}
async function fetchPageInfo(url) {
    try {
        const response = await fetch(url);
        const text = await response.text();
        const parser = new DOMParser();
        const doc = parser.parseFromString(text, 'text/html');
        const author = doc.querySelector('meta[name="author"]').getAttribute('content');
        const title = doc.querySelector('title').innerText.split(':')[0].trim();
        const teaserImg = doc.querySelector('.teaser-img img').getAttribute('src');
        return { author, title, teaserImg };
    } catch (error) {
        console.error('Error fetching page info:', error);
        return { author: 'Unknown', title: 'Unknown', teaserImg: 'preview-image.webp' };
    }
}


async function populateTiles2() {
    const presentations = await extractPresentations();
    const cardGrid = document.querySelector('.card-grid');
    if (!cardGrid) {
        return;
    }
    const sectionTemplate = `
        <div class="section-header block" data-tags="category{{classNumber}}">
            Class {{classNumber}}
        </div>
    `;

    // Iterate over all presentations and create a card for each

    for (const section of presentations) {
        for (const presentationInfo of section.sectionPresentations) {

            if (presentationInfo.visibility === false) {
//                console.log('Skipping invisible presentation:', presentationInfo.title);
                continue;
            }
            const gridItem = document.createElement('div');
            gridItem.classList.add('grid-item');
            // gridItem.setAttribute('data-tags', `category${classNumber}`);
            const card = document.createElement('div');
            card.classList.add('card');
            const front = document.createElement('div');
            front.classList.add('front', 'block');
            const back = document.createElement('div');
            back.classList.add('back', 'block');
            // const handoutLink = lecture.replace('-deck', '-handout');
            // Initially set content with placeholders
            front.innerHTML = `
            <img src="${presentationInfo.teaserImage || 'preview-image.webp'}" alt="Preview Image">
            <h2>${presentationInfo.title || 'Loading...'}</h2>
            <div class="card-content">
                <p>${presentationInfo.description || ''}</p>
            </div>
            `;




            back.innerHTML = `
            <div class="card-content">
                <h2>${presentationInfo.title || 'Loading...'}</h2>
                <p>${presentationInfo.description || ''}</p>
            </div>
            <div class="icons">
                <a href="${presentationInfo.url || '#'}"><img src="/support/vendor/images/theme/presentation-icon.svg" alt="Presentation"></a>
            </div>
            <p class="author-info">${presentationInfo.date || 'Date'}, ${presentationInfo.author || 'Author'}</p>
            `;

            // There might be more than one supplemental. These are given in the supplimentals array  - we need to iterate over them and add them to the icons object on the back of the card

            if (presentationInfo.supplemental.length > 0) {
                for (const supplemental of presentationInfo.supplemental) {
                    //create an attribute for the supplemental link, including the url, icon as image source 
                    const supplementalLink = document.createElement('a');
                    supplementalLink.href = supplemental.url;
                    supplementalLink.target = '_blank';
                    const supplementalIcon = document.createElement('img');
                    supplementalIcon.src = supplemental.affiliationLog || 'https://img.icons8.com/?size=50&id=y7PxIZBtQmoP&format=png';
                    supplementalIcon.classList.add('supplemental-icon');
                    // const supplementalText = document.createElement('span');
                    // supplementalText.classList.add('supplemental-text');
                    // supplementalText.innerText = supplemental.title;
                    supplementalLink.appendChild(supplementalIcon);
                    //supplementalLink.appendChild(supplementalText);
                    back.querySelector('.icons').appendChild(supplementalLink);
                }

                // Append elements to the card and grid item
                card.appendChild(front);
                card.appendChild(back);
                gridItem.appendChild(card);
                cardGrid.appendChild(gridItem);

                front.querySelector('img').src = presentationInfo.teaserImage;
                front.querySelector('h2').innerText = presentationInfo.title;

                back.querySelector('h2').innerText = presentationInfo.title;
                // back.querySelector('.author-info').innerText = `Date, ${presentationInfo.author}`;

                // add keyword tags to the front of the card as badges 
                if (presentationInfo.keywords.length > 0) {
                    const keywordContainer = document.createElement('div');
                    keywordContainer.style.display = 'flex';
                    keywordContainer.style.alignItems = 'flex-start';
                    keywordContainer.style.flexWrap = 'wrap';

                    keywordContainer.classList.add('keyword-container');
                    for (const keyword of presentationInfo.keywords) {
                        const keywordBadge = document.createElement('span');
                        keywordBadge.classList.add('badge', 'badge-pill', 'badge-secondary', 'keyword');
                        keywordBadge.innerText = keyword;
                        keywordContainer.appendChild(keywordBadge);
                    }
                    front.appendChild(keywordContainer);
                }

                //add the keywords as filter tags to the grid item
                gridItem.setAttribute('data-tags', presentationInfo.keywords.join(' '));


            }
        }
        ;

    }
}



async function populateTiles() {
    const tileContainer = document.getElementById('tileContainer');
    if (!tileContainer) {
        return;
    }
    const presentations = await extractPresentations();
    const sectionTemplate = `
        <div class="section-description">
            <h2 class="section-title">{{sectionTitle}}</h2>
        </div>
    `;
    const chapterTitleTemplate = `
        <div class="chapter-title">{{chapter}}</div>
    `;
    const cardTemplate = `
        <div class="card chapter">
            <a href="{{url}}" class="card-link">
                <img src="{{teaserImage}}" class="card-img-top" alt="...">
                <div class="card-body">
                    <h5 class="card-title">{{chapter}} {{section}} {{title}}</h5>
                </div>
            </a>
            <div class="card-links">
                {{#handout}}
                <a href="{{handout}}" target="_blank" class="handout-link">
                    <img src="https://img.icons8.com/?size=50&id=y7PxIZBtQmoP&format=png" class="handout-icon">
                    <span class="handout-text">Handout</span>
                </a>
                {{/handout}}
                {{#supplemental.length}}
                {{#supplemental}}
                <a href="{{url}}" target="_blank" class="supplemental-link">
                    <img src="{{affiliationLogo}}" class="supplemental-icon">
                    <span class="supplemental-text">{{title}}</span>
                </a>
                {{/supplemental}}
                {{/supplemental.length}}
            </div>
            <div class="keyword-container">
                {{#keywords}}
                <span class="badge badge-pill badge-secondary keyword">{{.}}</span>
                {{/keywords}}
            </div>
        </div>
    `;
    presentations.forEach(section => {
        const sectionDiv = document.createElement('div');
        sectionDiv.classList.add('section-description');
        const sectionTitle = document.createElement('h2');
        sectionTitle.classList.add('section-title');
        sectionTitle.textContent = section.sectionTitle || '';
        sectionDiv.appendChild(sectionTitle);
        tileContainer.appendChild(sectionDiv);
        let previousChapter = '';
        section.sectionPresentations.forEach(presentation => {
            const chapterTitle = presentation.chapter;
            if (previousChapter !== chapterTitle) {
                const renderedChapterTitle = Mustache.render(chapterTitleTemplate, { chapter: chapterTitle });
                tileContainer.appendChild(parseHTML(renderedChapterTitle));
                previousChapter = chapterTitle;
            }
            const renderedCard = Mustache.render(cardTemplate, presentation);
            tileContainer.appendChild(parseHTML(renderedCard));
        });
    });
}
function parseHTML(html) {
    const div = document.createElement('div');
    div.innerHTML = html;
    return div;
}
function removeDefaultLayout() {
    let defaultLayout = document.getElementsByClassName('default-layout')[0];
    if (defaultLayout) {
        defaultLayout.parentNode?.removeChild(defaultLayout);
    }
}
// Attach the filterTiles function to the filter buttons
function addFilters() {
    // Get all chapter buttons and add event listeners
    const filterButtons = document.querySelectorAll('.js-button-filter.chapter-button');
    filterButtons.forEach(button => {
        button.addEventListener('click', function () {
            // Toggle the active class
            this.classList.toggle('button--is-active');
            // Get active categories
            const activeCategories = Array.from(filterButtons)
                .filter(button => button.classList.contains('button--is-active'))
                .map(button => button.getAttribute('data-category'));
            //if no category is active, show all cards
            if (activeCategories.length === 0) {
                const cards = document.querySelectorAll('.card:not(.filter-column)');
                cards.forEach(card => {
                    card.style.display = 'block';
                });
                return;
            }
            // Filter cards based on active categories 
            // Igonore cards that have filter-column as class to avoid filtering the filter buttons
            const cards = document.querySelectorAll('.card:not(.filter-column)');
            cards.forEach(card => {
                const cardCategory = card.getAttribute('data-category');
                if (activeCategories.includes(cardCategory)) {
                    card.style.display = 'block';
                }
                else {
                    card.style.display = 'none';
                }
            });
        });
    });
    // Get all keyword buttons and add event listeners
    const keywordButtons = document.querySelectorAll('.js-button-filter.keyword-button');
    keywordButtons.forEach(button => {
        button.addEventListener('click', function () {
            // Toggle the active class
            this.classList.toggle('button--is-active');
            // Get active keywords
            const activeKeywords = Array.from(keywordButtons)
                .filter(button => button.classList.contains('button--is-active'))
                .map(button => button.getAttribute('data-category'));
            //if no keyword is active, show all cards
            if (activeKeywords.length === 0) {
                const cards = document.querySelectorAll('.card:not(.filter-column)');
                cards.forEach(card => {
                    card.style.display = 'block';
                });
                return;
            }
            // Filter cards based on active keywords
            // Igonore cards that have filter-column as class to avoid filtering the filter buttons
            const cards = document.querySelectorAll('.card:not(.filter-column)');
            cards.forEach(card => {
                const cardKeywords = card.getAttribute('data-keywords');
                // If the card has no keywords, it should be filtered out if any keyword is active  
                if (!cardKeywords) {
                    if (activeKeywords.length > 0) {
                        card.style.display = 'none';
                    }
                    else {
                        card.style.display = 'block';
                    }
                    return; // continue with next card
                }
                if (activeKeywords.some(keyword => {
                    if (keyword) {
                        cardKeywords.includes(keyword);
                    }
                })) {
                    card.style.display = 'block';
                }
                else {
                    card.style.display = 'none';
                }
            });
        });
    });
}
async function main() {
    populateTiles2();
    createChapterFilterButtons();
    createTagFilterButtons();
    addFilters();
    // removeDefaultLayout();
}
// Run the main function when the DOM is ready or immediately if it is already ready
if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', main);
} else {
    main();
}