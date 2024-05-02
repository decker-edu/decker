function initIndexPage() {
  setupProgressIndicators();
  loadSources();
}

/* Index Pages should be small enough that loading all sources at once
 * instead of loading with an intersection observer should be feasable.
 */
function loadSources() {
  const sources = document.querySelectorAll("[data-src]");
  for (const source of sources) {
    source.setAttribute("src", source.getAttribute("data-src"));
  }
}

function setupProgressIndicators() {
  if (!localStorage) return;

  // Scrape settings from Decker meta
  if (!Decker.meta.progress) return;
  let selector = Decker.meta.progress.selector || "a[href$='-deck.html']";
  let insert = Decker.meta.progress.insert || "after";

  document.querySelectorAll(selector).forEach((link) => {
    let url = null;
    try {
      url = new URL(link.href);
    } catch {
      return;
    }

    const key = url.pathname + "-percentage";
    let percent = localStorage.getItem(key) || 0;
    // Reset if percent is faulty
    percent = Number(percent);
    if (isNaN(percent) || percent === Infinity || percent > 100) {
      console.log(
        "[index.js] reset percent progress for " +
          key +
          " because value is " +
          percent
      );
      percent = 0;
      localStorage.setItem(key, 0);
    }

    let progress = document.createElement("progress");
    progress.max = 100;
    progress.value = percent;
    progress.key = key;
    progress.title = `${percent}% watched.\nDouble-click to toggle\nbetween 100% and 0%`;

    progress.ondblclick = function () {
      this.value = this.value == 100 ? 0 : 100;
      this.title = `${this.value}% watched.\nDouble-click to toggle\nbetween 100% and 0%`;
      localStorage.setItem(this.key, this.value);
    };

    if (insert === "replace") {
      link.replaceWith(progress);
    } else if (insert == "before") {
      link.before(progress);
    } else {
      link.after(progress);
    }
  });
}
