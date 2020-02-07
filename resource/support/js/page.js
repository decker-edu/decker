
function initPage() {
    addSourceCodeLabels();
    addBootstrapTableClasses();
}

function addSourceCodeLabels() {
    $("div.sourceCode[label]").each(function () {
        $("<div/>")
            .addClass("language-label")
            .text($(this).attr("label"))
            .prependTo($(this).children('pre'));
    });
}

function addBootstrapTableClasses() {
    $("table").addClass(
        "table table-sm table-responsive"
    );
}