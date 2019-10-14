// import $ from "jquery";
// require("@fortawesome/fontawesome-free/css/all.css");
// require("@fortawesome/fontawesome-free/js/all");
// require("bootstrap/dist/css/bootstrap.css");
// require("./handout.scss");

addSourceCodeLabels();

document.addEventListener("load", () => {
  $("table").addClass(
    "table table-striped table-bordered table-hover table-condensed table-responsive"
  );
});

function addSourceCodeLabels() {
  $("div.sourceCode[label]").each(function () {
    $("<div/>")
      .addClass("language-label")
      .text($(this).attr("label"))
      .prependTo($(this).children('pre'));
  });
}