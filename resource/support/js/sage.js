"use strict";

/*
 * Convert HTML code containing SAGE cells into a data-URL 
 * containing the whole SAGE page (HTML, CSS, JS)
 */
function build_sage_url(html)
{
    const source = String.raw`<!DOCTYPE html>
<html>
  <head>
    <meta http-equiv="content-type" content="text/html; charset=UTF-8">
    <meta charset="UTF-8">
  </head>
  <body>
  <style>
    html,body {
        background-color: white;
        border:           0px;
        padding:          0px;
        margin:           0px;
        overflow-x:       hidden;
        overflow-y:       auto;
        width:            100vw;
        height:           100vh;
        font-size:        24px;
    }
    body {
        padding:    10px;
        box-sizing: border-box;
    }
    .sagecell {
        font-size: 20px;
    }
    button.sagecell_evalButton {
        font-size:     20px;
        margin-bottom: 30px;
        outline:       none;
    }
    .sagecell .CodeMirror-scroll {
        max-height: 100em !important;
    }
    </style>
    <script src="https://sagecell.sagemath.org/static/embedded_sagecell.js"></script>
    <script>
    sagecell.makeSagecell({ inputLocation: 'div.compute.sage',
                            evalButtonText: 'Evaluate',
                            linked: true,
                            languages: ['sage', 'python', 'r', 'octave'],
                            hide: ['fullScreen'] });
    sagecell.makeSagecell({ inputLocation: 'div.compute.python',
                            evalButtonText: 'Evaluate',
                            linked: true,
                            languages: ['python', 'sage', 'r', 'octave'],
                            hide: ['fullScreen'] });
    sagecell.makeSagecell({ inputLocation: 'div.compute.rr',
                            evalButtonText: 'Evaluate',
                            linked: true,
                            languages: ['r', 'sage', 'python', 'octave'],
                            hide: ['fullScreen'] });
    sagecell.makeSagecell({ inputLocation: 'div.compute.octave',
                            evalButtonText: 'Evaluate',
                            linked: true,
                            languages: ['octave', 'sage', 'python', 'r'],
                            hide: ['fullScreen'] });
    </script>
${html}
</body>
</html>
`
    const blob = new Blob([source], { type: 'text/html' });
    return URL.createObjectURL(blob);
}


/*
 * Change <pre><code> blocks intro <div><script> blocks, as expected
 * by SAGE. Has to be done before highlight.js processes the code!
 * Then move SAGE cells to an iframe using data-URLs.
 */
function prepareSAGE()
{
    for (let sageCell of document.querySelectorAll('div.sageCell')) 
    {
        // don't know why this is necessary, but otherwise
        // MathJax rendering in SAGE cells does not work properly.
        for (let e of sageCell.querySelectorAll('.math')) 
        {
            e.classList.remove("math");
        }

        // replace <pre> by <div>.compute
        // replace <code> by <script>
        for (let code of sageCell.querySelectorAll('pre>code')) 
        {
            var pre = code.parentElement;

            var script = document.createElement("script");
            script.innerHTML = code.innerText; // don't use code.innerHTML, it's escaped
            script.type = "text/x-sage";

            var div = document.createElement("div");
            div.classList = pre.classList;
            div.classList.add("compute");
            div.appendChild(script);

            pre.replaceWith(div);
        }

        // construct data-url for whole SAGE page
        var url = build_sage_url(sageCell.innerHTML);

        // construct iframe with data-url as data-src, such that
        // it be lazy-loaded by Reveal
        var iframe = document.createElement("iframe");
        iframe.classList.add("decker");
        if (sageCell.classList.contains("stretch")) {
            iframe.classList.add("stretch");
            sageCell.classList.remove("stretch");
        }
        else { 
            iframe.style.width  = sageCell.getAttribute('width')  || sageCell.style.width  || "100%";
            iframe.style.height = sageCell.getAttribute('height') || sageCell.style.height || "500px";
        }
        if (sageCell.classList.contains("print")) {
            iframe.classList.add("print");
        }
        iframe.sandbox = 'allow-scripts allow-same-origin';
        iframe.setAttribute("data-src", url);
    
        // don't need original sageCell anymore
        sageCell.replaceWith(iframe);
    }
}

