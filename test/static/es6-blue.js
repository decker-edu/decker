// import * as mod from "./es6.js";

let anchor = document.getElementById(new URL(import.meta.url).hash.slice(1));

function rainbow(numOfSteps, step) {
  // This function generates vibrant, "evenly spaced" colours (i.e. no clustering). This is ideal for creating easily distinguishable vibrant markers in Google Maps and other apps.
  // Adam Cole, 2011-Sept-14
  var r, g, b;
  var h = step / numOfSteps;
  var i = ~~(h * 6);
  var f = h * 6 - i;
  var q = 1 - f;
  switch (i % 6) {
    case 0:
      r = 1;
      g = f;
      b = 0;
      break;
    case 1:
      r = q;
      g = 1;
      b = 0;
      break;
    case 2:
      r = 0;
      g = 1;
      b = f;
      break;
    case 3:
      r = 0;
      g = q;
      b = 1;
      break;
    case 4:
      r = f;
      g = 0;
      b = 1;
      break;
    case 5:
      r = 1;
      g = 0;
      b = q;
      break;
  }
  var c =
    "#" +
    ("00" + (~~(r * 255)).toString(16)).slice(-2) +
    ("00" + (~~(g * 255)).toString(16)).slice(-2) +
    ("00" + (~~(b * 255)).toString(16)).slice(-2);
  return c;
}

function step(n) {
  let color = rainbow(8, n);
  anchor.innerHTML = `<svg viewBox="0 0 200 100" style="background-color:${color};width:100%;"></svg>`;
  setTimeout(step, 500, (n + 1) % 8);
}

step(0);