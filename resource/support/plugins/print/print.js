"use strict";

var RevealPrint = (function(){

    /* this function is called inbetween just before Reveal's setupPDF.
     * We setup the minimum height of slide elements to Reveal's page height.
     */
    function setHeight()
    {
        if (!Reveal.getConfig().center)
        {
            var height = Reveal.getConfig().height;
            Reveal.getSlides().forEach( function( slide ) {
                slide.style.height = height + "px";
            });
        }
    }


    // pandoc puts footers into a <p> element, which
    // makes positioning w.r.t. slide bottom difficult.
    // hence we remove these p-elements and put the
    // footers as children of the slide element
    function fixFooters()
    {
		Reveal.getSlides().forEach( function( slide ) {
            var footers = slide.getElementsByClassName('footer');
            for (var i=0; i<footers.length; i++)
            {
                var footer = footers[i];
                var parent = footer.parentElement;
                if (parent.nodeName == "P")
                {
                    slide.appendChild(footer);
                    if (parent.childNodes.length == 0) {
                        parent.parentElement.removeChild(parent);
                    }
                }
            }
        });
    }


    /* check whether the current slide is too tall and print error in that case */
    function checkHeight()
    {
        var configHeight  = Reveal.getConfig().height;
        var slide         = Reveal.getCurrentSlide();
        var slideHeight   = slide.clientHeight;

        if (slideHeight > configHeight)
        {
            console.warn("slide " + slideNumber() + " is " + (slideHeight-configHeight) + "px too high");
            slide.style.border = "1px dashed red";
        }
        else
        {
            slide.style.border = '';
        }
    }


    /* return string with current slide number */
    function slideNumber()
    {
        var idx = Reveal.getIndices();
        return idx.v>0 ? "(h:" + (idx.h+1) + ", v:" + (idx.v+1) + ")" : idx.h+1;
    }


    /* load iframes that are marked with class 'pdf'.
     * used to include simple Javascript demos in PDF
     */
    function setupIframes()
    {
        for (var e of document.querySelectorAll('.reveal section figure.print iframe, .reveal section iframe.print'))
        {
            if (e.hasAttribute("data-src"))
            {
                e.src = e.getAttribute("data-src");
            }
        }
    }


    /* 
     * Remove controls from videos, since they mess up printing.
     * If we have >5 videos and are printing from headless Chrome,
     * this will stall due a Chrome bug. Hence we disable videos
     * from the sixth video on in this configuration.
     */
    function setupVideos()
    {
        let numVideos = 0;
        let maxVideos = 5; // headless Chrome can handle 5 videos
        
        // go through all slides
        for (let slide of document.getElementsByTagName("section"))
        {
            // do we have a background video?
            if (slide.hasAttribute("data-background-video"))
            {
                // handle headless Chrome bug
                if (navigator.webdriver && numVideos >= maxVideos)
                {
                    slide.removeAttribute("data-background-video");
                }

                // play video to 0.5s to get a poster frame
                else
                {
                    var src = slide.getAttribute("data-background-video");
                    if (!src.includes("#t="))
                    {
                        src = src + "#t=0.5";
                        slide.setAttribute("data-background-video", src);
                    }
                }

                numVideos++;
            }

            // do we have videos on this slide?
            for (let video of slide.getElementsByTagName("video"))
            {
                // handle headless Chrome bug
                if (navigator.webdriver && numVideos >= maxVideos)
                {
                    video.setAttribute("preload", "none");
                    video.style.border = "1px dashed red";
                }

                // play video to 0.5s to get a poster frame
                else
                {
                    if (video.hasAttribute("data-src"))
                    {
                        var src = video.getAttribute("data-src");
                        if (!src.includes("#t="))
                            src = src + "#t=0.5";

                        video.src = src;
                        video.removeAttribute("data-src");
                    }

                    video.removeAttribute("controls");
                    video.removeAttribute("data-autoplay");
                    video.removeAttribute("autoplay");
                }

                numVideos++;
            }
        }
    }


    // set title, such that the exported PDF has the same filename
    function setupTitle()
    {
        var url = window.location.pathname;
        var filename = url.substring(url.lastIndexOf('/')+1);
        var basename = filename.substring(0, url.lastIndexOf("."));
        document.title = basename + "pdf";
    }


	return {
		init: function() { 
            return new Promise( function(resolve) {
                Reveal.addEventListener( 'ready', fixFooters );
                Reveal.addEventListener( 'ready', setHeight  );
   
                if (Reveal.getConfig().checkOverflow)
                {
                    Reveal.addEventListener( 'slidechanged', checkHeight );
                }

                /* are we exporting a PDF? */
                var pdf = !!window.location.search.match(/print-pdf/gi);
                if (pdf)
                {
                    setupIframes();
                    setupVideos();
                    setupTitle();

                    // automatically press the print button when not in headless mode
                    if (!navigator.webdriver && !isElectron())
                    {
                        Reveal.addEventListener( 'pdf-ready', function() {
                            setTimeout( window.print, 1000 );
                        });
                    }
                }

                resolve();
            });
        }
    }

})();

Reveal.registerPlugin( 'print', RevealPrint );

