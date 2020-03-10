
---
title: 'Decker Media Filter - Test Report'
---

Introduction
============

This report is generated during testing and shows the HTML output for a representative selection of image tags. It is used for debugging and is the authoritative reference for CSS authors.

<div>

------------------------------------------------------------------------

Plain image
-----------

An image that is used inline in a paragraph of text.

``` {.markdown}
![](/some/path/image.png)
```

translates to

``` {.html}
<img class="decker" data-src="some/path/image.png">
```

------------------------------------------------------------------------

Plain image with caption
------------------------

An image with a caption. The image is surrounded by a figure element.

``` {.markdown}
![](path/image.png)

Caption: Caption.
```

translates to

``` {.html}
<figure class="decker">
    <img class="decker" data-src="path/image.png" style="width:100%;">
    <figcaption class="decker">
         
        Caption.
    </figcaption>
</figure>
```

------------------------------------------------------------------------

Plain image with URL query
--------------------------

Query string and fragment identifier in URLs are preserved.

``` {.markdown}
![Caption.](https://some.where/image.png&key=value)
```

translates to

``` {.html}
<figure class="decker">
    <img class="decker" data-src="https://some.where/image.png&key=value" style="width:100%;">
    <figcaption class="decker">
        Caption.
    </figcaption>
</figure>
```

------------------------------------------------------------------------

Plain image with custom attributes.
-----------------------------------

Image attributes are handled in complex ways.

``` {.markdown}
![Caption.](/some/path/image.png){#myid .myclass width="40%" css:border="1px" myattribute="value"}
```

translates to

``` {.html}
<figure id="myid" class="decker myclass" data-myattribute="value" style="width:40%;border:1px;">
    <img class="decker" data-src="some/path/image.png" style="width:100%;">
    <figcaption class="decker">
        Caption.
    </figcaption>
</figure>
```

------------------------------------------------------------------------

Plain video
-----------

Images that are videos are converted to a video tag.

``` {.markdown}
![Caption.](/some/path/video.mp4){width="42%"}
```

translates to

``` {.html}
<figure class="decker" style="width:42%;">
    <video class="decker" data-src="some/path/video.mp4" style="width:100%;">
        
    </video>
    <figcaption class="decker">
        Caption.
    </figcaption>
</figure>
```

------------------------------------------------------------------------

Plain video with Media Fragments URI
------------------------------------

A local video with start time.

``` {.markdown}
![Caption.](/some/path/video.mp4){start="5" stop="30" preload="none"}
```

translates to

``` {.html}
<figure class="decker">
    <video class="decker" data-src="some/path/video.mp4#t=5,30" style="width:100%;" preload="none">
        
    </video>
    <figcaption class="decker">
        Caption.
    </figcaption>
</figure>
```

------------------------------------------------------------------------

Plain video with specific attributes
------------------------------------

Video tag specific classes are translated to specific attributes.

``` {.markdown}
![Caption.](/some/path/video.mp4){.controls .autoplay start="5" stop="30" poster="somewhere/image.png" preload="none"}
```

translates to

``` {.html}
<figure class="decker">
    <video class="decker" data-src="some/path/video.mp4#t=5,30" style="width:100%;" poster="somewhere/image.png" preload="none" controls="1" autoplay="1">
        
    </video>
    <figcaption class="decker">
        Caption.
    </figcaption>
</figure>
```

------------------------------------------------------------------------

Three images in a row
---------------------

Line blocks filled with only image tags are translated to a row of images. Supposed to be used with a flexbox masonry CSS layout.

``` {.markdown}
| ![](image.png)
| ![Caption.](movie.mp4){.autoplay}
| ![](image.png){css:border=\"1px black\"}
```

translates to

``` {.html}
<figure style="border:2px solid cyan;">
    <div class="image-row">
        <div>
            <img class="decker" data-src="image.png">
        </div>
        <div>
            <figure class="decker">
                <video class="decker" data-src="movie.mp4" style="width:100%;" autoplay="1">
                    
                </video>
                <figcaption class="decker">
                    Caption.
                </figcaption>
            </figure>
        </div>
    </div>
    <figcaption>
        <img src="image.png" />
        {css:border=
        "
        1px
         
        black
        "
        }
    </figcaption>
</figure>
```

------------------------------------------------------------------------

Iframe with caption
-------------------

A simple iframe with a caption. The URL can be a top level domain because the \`iframe\` class is specified.

``` {.markdown}
![Caption.](https://www.heise.de/){.iframe}
```

translates to

``` {.html}
<figure class="decker iframe">
    <iframe class="decker" allow="fullscreen" data-src="https://www.heise.de/" style="width:100%;">
        
    </iframe>
    <figcaption class="decker">
        Caption.
    </figcaption>
</figure>
```

------------------------------------------------------------------------

Iframe with custom attributes and query string
----------------------------------------------

A simple iframe with custom attributes and a query string that are both transfered correctly.

``` {.markdown}
![Caption.](https://www.heise.de/index.html#some-frag?token=83fd3d4){height="400px" model="some-stupid-ass-model.off" lasersword="off"}
```

translates to

``` {.html}
<figure class="decker">
    <iframe class="decker" allow="fullscreen" data-src="https://www.heise.de/index.html#some-frag?token=83fd3d4" data-model="some-stupid-ass-model.off" data-lasersword="off" style="width:100%;height:400px;">
        
    </iframe>
    <figcaption class="decker">
        Caption.
    </figcaption>
</figure>
```

------------------------------------------------------------------------

Mario\'s model viewer
---------------------

A simple iframe with a special url.

``` {.markdown}
![Caption.](http://3d.de/model.off){.mario height="400px" phasers="stun"}
```

translates to

``` {.html}
<figure class="decker mario">
    <iframe class="decker" allow="fullscreen" data-src="/support/vendor/mview/mview.html" data-model="http://3d.de/model.off" data-phasers="stun" style="width:100%;height:400px;">
        
    </iframe>
    <figcaption class="decker">
        Caption.
    </figcaption>
</figure>
```

</div>
