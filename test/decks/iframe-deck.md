---
title: Iframes
---

# Iframes

## Some slides with iframes

1.  Iframe embedded in slide
2.  Iframe as background
3.  Iframe embedded in slide showing a PDF
4.  Iframe as background showing a PDF

------------------------------------------------------------------------

# Iframe showing <https://hci.uni-wuerzburg.de/>

![](https://hci.uni-wuerzburg.de/){.iframe width="100%" height="500px"}

------------------------------------------------------------------------

# ![](https://hci.uni-wuerzburg.de/){.iframe}

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

------------------------------------------------------------------------

# Iframe showing local HTML file

## While preserving the URI query string

![](../reload.html?f=x%2By&g=&x_min=-1&x_max=1&x_steps=100&y_min=-1&y_max=1&y_steps=100&z_min=0&z_max=0&ncontours=60&heatmap_coloring=0){.iframe .resource}

-   Iframe source should be

``` {.html}
<iframe 
    data-src="../reload.html?f=x%2By&g=&x_min=-1&x_max=1&x_steps=100&y_min=-1&y_max=1&y_steps=100&z_min=0&z_max=0&ncontours=60&heatmap_coloring=0" 
    class="iframe 
    resource">
Browser does not support iframe.
</iframe>
```

