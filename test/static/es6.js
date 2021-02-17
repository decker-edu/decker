export { hello };

function hello(div, color) {
  div.setAttribute(
    "style",
    `width:200px; height:100px; background-color: ${color};`
  );
}
