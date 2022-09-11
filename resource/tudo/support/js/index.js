function initIndexPage() {
  setupProgressIndicators();
}

function setupProgressIndicators() {
  if (!localStorage) return;

  document.querySelectorAll("a[href$='-deck.html']").forEach((link) => {
    const url = new URL(link.href);
    const key = url.pathname + "-percentage";
    const percent = localStorage.getItem(key) || 0;

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

    link.parentElement.insertBefore(progress, link.nextSibling);
  });
}
