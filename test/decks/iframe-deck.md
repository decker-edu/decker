---
history: true
title: Iframes
---

# Iframes

## Some slides with iframes

1.  Iframe embedded in slide
2.  Iframe as background
3.  Iframe embedded in slide showing a PDF
4.  Iframe as background showing a PDF

------------------------------------------------------------------------

# Iframe showing <https://www.uni-wuerzburg.de/startseite>

![This is the most ugly homepage
ever.](https://www.uni-wuerzburg.de/startseite.html?some-option=some-value){.iframe
width="100%" height="400px" model="some-model.off"}

------------------------------------------------------------------------

# ![](https://www.uni-wuerzburg.de/startseite.html)

------------------------------------------------------------------------

# Iframe showing <https://pandoc.org/MANUAL.pdf>

![](https://pandoc.org/MANUAL.pdf){width="100%" height="500px"}

------------------------------------------------------------------------

# ![](include/06-metal.pdf)

------------------------------------------------------------------------

# Iframe showing local HTML file

## While preserving the URI query string

![](../reload.html?model=../../meshes/blubb.off){.iframe .resource}

-   Iframe source should be

``` {.html}
<iframe 
    data-src="../reload.html?model=../../meshes/blubb.off" 
    class="iframe 
    resource">
Browser does not support iframe.
</iframe>
```
