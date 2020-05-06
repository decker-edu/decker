
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
![$e=mc^2$](/test/decks/include/06-metal.png)
```

translates to

``` {.html}
<figure class="decker">
    <img class="decker" data-src="test/decks/include/06-metal.png">
    <figcaption class="decker">
        <span class="math inline">
            <em>
                e
            </em>
             
            =
             
            <em>
                m
            </em>
            <em>
                c
            </em>
            <sup>
                2
            </sup>
        </span>
    </figcaption>
</figure>
```

------------------------------------------------------------------------

SVG image
---------

An SVG image that is embedded into the HTML document.

``` {.markdown}
![](/test/decks/empty.svg){.embed css:background-color="magenta"}
```

translates to

``` {.html}
<span class="decker svg embed" style="background-color:magenta;">
    <svg>This space intentionally left blank</svg>

</span>
```

------------------------------------------------------------------------

Embedded PDF
------------

A PDF document that is embedded through an object tag.

``` {.markdown}
![](https://adobe.com/some.pdf)
```

translates to

``` {.html}
<object class="decker" type="application/pdf" data="https://adobe.com/some.pdf">
    
</object>
```

------------------------------------------------------------------------

Plain image with caption
------------------------

An image with a caption. The image is surrounded by a figure element.

``` {.markdown}
![Caption.](/test/decks/include/06-metal.png)

This is not a caption, but the next paragraph.
```

translates to

``` {.html}
<figure class="decker">
    <img class="decker" data-src="test/decks/include/06-metal.png">
    <figcaption class="decker">
        Caption.
    </figcaption>
</figure>

This is not a caption, but the next paragraph.
```

------------------------------------------------------------------------

Plain image with caption
------------------------

An image with a caption. The image is surrounded by a figure element.

``` {.markdown}
![](/test/decks/include/06-metal.png)

Caption: Caption.
```

translates to

``` {.html}
<figure class="decker">
    <img class="decker" data-src="test/decks/include/06-metal.png">
    <figcaption class="decker">
         
        Caption.
    </figcaption>
</figure>
```

------------------------------------------------------------------------

Plain image with caption
------------------------

An image with a caption containg a citation.

``` {.markdown}
![](/test/decks/include/06-metal.png)

Caption: Caption [see @tramberend2003].
```

translates to

``` {.html}
<figure class="decker">
    <img class="decker" data-src="test/decks/include/06-metal.png">
    <figcaption class="decker">
         
        Caption
         
        <span class="citation" data-cites="tramberend2003">
            (see
             
            Tramberend
             
            2003)
        </span>
        .
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
    <img class="decker" data-src="https://some.where/image.png&key=value">
    <figcaption class="decker">
        Caption.
    </figcaption>
</figure>
```

------------------------------------------------------------------------

Plain image with size attributes.
---------------------------------

 Percentage values for \`width\` and \`height\` are transfered to the figure element, other values go to the image element.

``` {.markdown}
![Caption.](/test/decks/include/06-metal.png){width="40%"}
```

translates to

``` {.html}
<figure class="decker" style="width:40%;">
    <img class="decker" data-src="test/decks/include/06-metal.png">
    <figcaption class="decker">
        Caption.
    </figcaption>
</figure>
```

------------------------------------------------------------------------

Plain image with size attributes.
---------------------------------

 Percentage values for \`width\` and \`height\` are transfered to the figure element, other values go to the image element.

``` {.markdown}
![Caption.](/test/decks/include/06-metal.png){height="200px"}
```

translates to

``` {.html}
<figure class="decker">
    <img class="decker" data-src="test/decks/include/06-metal.png" style="height:200px;">
    <figcaption class="decker">
        Caption.
    </figcaption>
</figure>
```

------------------------------------------------------------------------

Plain image with size attributes.
---------------------------------

 Percentage values for \`width\` and \`height\` are transfered to the figure element, other values go to the image element.

``` {.markdown}
![Caption.](/test/decks/include/06-metal.png){height="200px" width="40%"}
```

translates to

``` {.html}
<figure class="decker" style="width:40%;">
    <img class="decker" data-src="test/decks/include/06-metal.png" style="height:200px;">
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
![Caption.](/test/decks/include/06-metal.png){#myid .myclass width="40%" css:border="1px" css:background-color="magenta" myattribute="value"}
```

translates to

``` {.html}
<figure id="myid" class="decker myclass" data-myattribute="value" style="width:40%;border:1px;background-color:magenta;">
    <img class="decker" data-src="test/decks/include/06-metal.png">
    <figcaption class="decker">
        Caption.
    </figcaption>
</figure>
```

------------------------------------------------------------------------

Plain audio
-----------

Images that are audio clips are converted to an audio tag.

``` {.markdown}
![Caption.](test/decks/audio.mp3){.autoplay .controls}
```

translates to

``` {.html}
<figure class="decker">
    <audio class="decker" data-src="test/decks/audio.mp3" controls="1" data-autoplay="1">
        
    </audio>
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
![Caption.](test/decks/pacman-perfect-game.mp4){width="42%"}
```

translates to

``` {.html}
<figure class="decker" style="width:42%;">
    <video class="decker" data-src="test/decks/pacman-perfect-game.mp4">
        
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
![Caption.](test/decks/pacman-perfect-game.mp4){start="5" stop="30" preload="none"}
```

translates to

``` {.html}
<figure class="decker">
    <video class="decker" data-src="test/decks/pacman-perfect-game.mp4#t=5,30" preload="none">
        
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
![Caption.](test/decks/pacman-perfect-game.mp4){.controls .autoplay start="5" stop="30" poster="/test/decks/include/06-metal.png" preload="none"}
```

translates to

``` {.html}
<figure class="decker">
    <video class="decker" data-src="test/decks/pacman-perfect-game.mp4#t=5,30" poster="/test/decks/include/06-metal.png" preload="none" controls="1" data-autoplay="1">
        
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
| ![](/test/decks/include/06-metal.png)
| ![Caption.](test/decks/pacman-perfect-game.mp4){.autoplay}
| ![](/test/decks/include/06-metal.png){css:border="1px solid black"}
```

translates to

``` {.html}
<div class="decker image-row">
    <img class="decker" data-src="test/decks/include/06-metal.png">

    <figure class="decker">
    <video class="decker" data-src="test/decks/pacman-perfect-game.mp4" data-autoplay="1">
        
    </video>
    <figcaption class="decker">
        Caption.
    </figcaption>
</figure>

</div>

| <img class="decker" data-src="test/decks/include/06-metal.png" style="border:1px solid black;">
```

------------------------------------------------------------------------

Four images in a row with caption
---------------------------------

Line blocks filled with only image tags are translated to a row of images. Supposed to be used with a flexbox masonry CSS layout.

``` {.markdown}
| ![](/test/decks/include/06-metal.png)
| ![](test/decks/pacman-perfect-game.mp4){.autoplay}
| ![](/test/decks/include/06-metal.png){css:border="1px solid black"}
| ![](/test/decks/include/06-metal.png)

Caption: Caption
```

translates to

``` {.html}
<div class="decker image-row">
    <img class="decker" data-src="test/decks/include/06-metal.png">

    <video class="decker" data-src="test/decks/pacman-perfect-game.mp4" data-autoplay="1">
    
</video>

</div>

| <img class="decker" data-src="test/decks/include/06-metal.png" style="border:1px solid black;">
 | <img class="decker" data-src="test/decks/include/06-metal.png">

Caption: Caption
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
    <iframe class="decker" allow="fullscreen" data-src="https://www.heise.de/">
        
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
    <iframe class="decker" allow="fullscreen" data-src="https://www.heise.de/index.html#some-frag?token=83fd3d4" data-model="some-stupid-ass-model.off" data-lasersword="off" style="height:400px;">
        
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
<div class="decker image error">
    <h2 class="title">
        <i class="fa fa-exclamation-triangle">
            
        </i>
         Decker error
    </h2>
    <p class="message">
        Local resource does not exist: support/mview/mview.html
    </p>
    <p>
        encountered while processing
    </p>
    <pre class="markup">
        <code class="markup">
            ![Caption.](http://3d.de/model.off){.mario height=&quot;400px&quot;
phasers=&quot;stun&quot;}

        </code>
    </pre>
</div>
```

------------------------------------------------------------------------

Youtube video stream
--------------------

An image with source URL scheme \`youtube:\` results in an embedded video player.

``` {.markdown}
![](youtube:1234567890){#video1 .autoplay .controls width="75%"}
```

translates to

``` {.html}
<div id="video1" class="decker" style="width:75%;">
    <div style="position:relative;padding-top:25px;padding-bottom:56.25%;height:0;">
        <iframe frameborder="0" allowfullscreen="1" style="position:absolute;top:0;left:0;width:100%;height:100%;" data-src="https://www.youtube.com/embed/1234567890?autoplay=1&cc_load_policy=0&controls=1&iv_load_policy=3&modestbranding=&rel=0&showinfo=0">
            Iframe showing video here.
        </iframe>
    </div>
</div>
```

------------------------------------------------------------------------

Vimeo it baby
-------------

An image with source URL scheme \`vimeo:\` results in an embedded video player.

``` {.markdown}
![Caption.](vimeo://1234567890){#video2 .some-class autoplay="1" aspect="4:3" width="75%" some-attribute="yeah"}
```

translates to

``` {.html}
<figure id="video2" class="decker some-class" data-some-attribute="yeah" style="width:75%;">
    <div style="position:relative;padding-top:25px;padding-bottom:75.00%;height:0;">
        <iframe frameborder="0" allowfullscreen="1" style="position:absolute;top:0;left:0;width:100%;height:100%;" data-src="https://player.vimeo.com/video/1234567890?autoplay=1&byline=0&controls=1&dnt=1&fun=0&title=0&transparent=false">
            Iframe showing video here.
        </iframe>
    </div>
    <figcaption class="decker">
        Caption.
    </figcaption>
</figure>
```

------------------------------------------------------------------------

Twitch it baby
--------------

An image with source URL scheme \`twitch:\` results in an embedded video player.

``` {.markdown}
![Caption.](twitch:1234567890){.autoplay .controls aspect="5:3" width="75%"}
```

translates to

``` {.html}
<figure class="decker controls" style="width:75%;">
    <div style="position:relative;padding-top:25px;padding-bottom:60.00%;height:0;">
        <iframe frameborder="0" allowfullscreen="1" style="position:absolute;top:0;left:0;width:100%;height:100%;" data-src="https://player.twitch.tv/?autoplay=1&video=1234567890">
            Iframe showing video here.
        </iframe>
    </div>
    <figcaption class="decker">
        Caption.
    </figcaption>
</figure>
```

------------------------------------------------------------------------

Background image
----------------

The last image in a level 1 header is promoted to the slide background.

``` {.markdown}
# Background Image ![](/test/decks/include/06-metal.png){size="cover"}
```

translates to

``` {.html}
<h1 data-background-size="cover" data-background-image="test/decks/include/06-metal.png" id="background-image" data-background-size="cover" data-background-image="test/decks/include/06-metal.png">Background Image  </h1>
```

------------------------------------------------------------------------

Background video
----------------

The last image in a level 1 header is promoted to the slide background.

``` {.markdown}
# Background Image ![](test/decks/pacman-perfect-game.mp4){.loop .muted color="black"}
```

translates to

``` {.html}
<h1 data-background-video-loop="1" data-background-video-muted="1" data-background-video="test/decks/pacman-perfect-game.mp4" id="background-image" data-background-video-loop="1" data-background-video-muted="1" data-background-video="test/decks/pacman-perfect-game.mp4">Background Image  </h1>
```

</div>
