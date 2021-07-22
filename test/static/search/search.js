export default setupSearch;

import FuzzySet from "./fuzzyset.js";

function setupSearch(anchor, minScore = 0.5) {
    fetch("/index.json")
        .then(res => res.json())
        .then(index => { setup(index, anchor, minScore); })
        .catch(err => console.log("cannot load: index.json", err));
}

function setup(index, anchor, minScore) {
    anchor.innerHTML = `
        <p>&#x1F9E0; <input class="search" placeholder="Looking for something?" type="text"></p>
        <p> <ol class="searchresult"></ol> </p>
    `;
    var search = anchor.querySelector("input");
    var results = anchor.querySelector("ol");
    var keys = Object.keys(index.index).map(k => k.toString());
    var fuzzy = FuzzySet(keys);
    search.addEventListener("keyup", (e) => {
        var matches = fuzzy.get(search.value, [], minScore);
        while (results.lastElementChild)
            results.removeChild(results.lastElementChild);
        for (var match of matches) {
            var word = match[1];
            var exact = search.value == word;
            var onSlides = index.index[word];
            for (var slide of onSlides) {
                var url = slide.slide;
                var count = slide.count;
                var sInfo = index.slides[url];
                var dInfo = index.decks[sInfo.deckUrl];
                var item = document.createElement("li");
                item.innerHTML = `${exact ? `<b>${word}</b>` : word} - &#x1F4D2; <a target="_blank" href="/${dInfo.deckUrl}">${dInfo.deckTitle}</a> - &#x1F4C3; <a target="_blank" href="/${url}">${sInfo.slideTitle}</a> (${count})`;
                results.appendChild(item);
            }
        }
    });
}

