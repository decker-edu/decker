/**
 * @param {String} HTML representing a single element
 * @return {Element}
 */
export function htmlToElement(html) {
  var template = document.createElement("template");
  html = html.trim(); // Never return a text node of whitespace as the result
  template.innerHTML = html;
  return template.content.firstChild;
}

/**
 * @param {String} HTML representing any number of sibling elements
 * @return {NodeList}
 */
export function htmlToElements(html) {
  var template = document.createElement("template");
  template.innerHTML = html;
  return template.content.childNodes;
}

/**
 * Tagged template literal function for coercing certain values to what
 * we would expected for a more JSX-like syntax. Also converts the string
 * to a DOM element.
 *
 * For values that we don't want to coerce, we just skip outputting them
 * Example:
 *   `class="${variable}"`
 * If the value of my variable was one of these types I don't want
 * JavaScript to coerce, then I'd get this:
 *   'class=""'
 */
export function jsx(strings, ...values) {
  let out = "";
  strings.forEach((string, i) => {
    const value = values[i];

    // Array - Join to string and output with value
    if (Array.isArray(value)) {
      out += string + value.join("");
    }
    // String - Output with value
    else if (typeof value === "string") {
      out += string + value;
    }
    // Number - Coerce to string and output with value
    // This would happen anyway, but for clarity's sake on what's happening here
    else if (typeof value === "number") {
      out += string + String(value);
    }
    // object, undefined, null, boolean - Don't output a value.
    else {
      out += string;
    }
  });
  return out;
}

// This script is released to the public domain and may be used, modified and
// distributed without restrictions. Attribution not necessary but appreciated.
// Source: https://weeknumber.net/how-to/javascript
Date.prototype.getWeek = function () {
  var date = new Date(this.getTime());
  date.setHours(0, 0, 0, 0);
  // Thursday in current week decides the year.
  date.setDate(date.getDate() + 3 - ((date.getDay() + 6) % 7));
  // January 4 is always in week 1.
  var week1 = new Date(date.getFullYear(), 0, 4);
  // Adjust to Thursday in week 1 and count number of weeks from date to week1.
  return (
    1 +
    Math.round(
      ((date.getTime() - week1.getTime()) / 86400000 -
        3 +
        ((week1.getDay() + 6) % 7)) /
        7,
    )
  );
};
