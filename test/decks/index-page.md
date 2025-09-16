---
lang: de-DE
subtitle: "**Custom Index Page Example**"
templates:
  vspace: |
    <div style="display:block; height::(url);"></div>
  assign: |
    <a href="assignments/:(url)-page.html">:(title)
  topic: |
    <a href="topics/:(url)-deck.html">:(title) 
title: Computergrafik Grundlagen
---

| Nr. | Datum  | Thema                                                               |
|:---:|:------:|:--------------------------------------------------------------------|
| 01  | 10.10. | **Einführung**                                                      |
|     |        | [@topic](000-introduction "Einführung in das Modul Computergrafik") |
|     |        | [@assign](a01 "Aufgabe 1")                                          |

[@vspace](10em)

``` {.javascript .run}
import("./" + Decker.meta.supportPath + "/fuzzySearch/search.js")
    .then((module) => {
      anchor.classList.add("search");
      anchor.innerHTML = `
        <div class="search-box">
          <i class="fa-solid fa-magnifying-glass"></i>
          <input class="search" placeholder="In Folien suchen" type="text">
        </div>
        <table class="search">
        <thead><tr><th>Wort</th><th>Foliensatz</th><th>Folie</th><th>Treffer</th></tr></thead>
        <tbody></tbody>
        </table>
      `;
      module.default(anchor, 0.6);
    });
```
