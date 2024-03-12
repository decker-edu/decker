let observer = undefined;

function initPage() {
  let options = {
    root: document.documentElement,
    rootMargin: "0px",
    threshold: 1.0,
  };

  observer = new IntersectionObserver(cameIntoView, options);
  let targets = document.querySelectorAll("[data-src]");
  for (const target of targets) {
    observer.observe(target);
  }
}

function cameIntoView(entries, observer) {
  entries.forEach((entry) => {
    if (entry.isIntersecting) {
      entry.setAttribute("src", entry.getAttribute("data-src"));
    }
  });
}
