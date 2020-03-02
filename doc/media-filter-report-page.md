Decker Media Filter - Test Report
=================================

This report is generated during \<pre\>\<code\>make test\</code\>\</pre\> and shows the HTML output for a representative selection of image tags. It is used for debugging and is the authoritative reference for CSS authors.

<div>

------------------------------------------------------------------------

``` {.markdown}
Inline ![](/some/path/image.png)
```

translates to

``` {.html}
<p>Inline <img class="decker" data-src="/some/path/image.png">
</p>
```

------------------------------------------------------------------------

``` {.markdown}
Inline ![This is a **plain** image.](/some/path/image.png)
```

translates to

``` {.html}
<p>Inline <figure class="decker">
    <img class="decker" data-src="/some/path/image.png">
    <figcaption class="decker">
        This
         
        is
         
        a
         
        <strong>
            plain
        </strong>
         
        image.
    </figcaption>
</figure>
</p>
```

------------------------------------------------------------------------

``` {.markdown}
Block

![](/some/path/image.png)

Image: This is a **plain** image.
```

translates to

``` {.html}
<p>Block</p>
<figure class="decker">
    <img class="decker" data-src="/some/path/image.png">
    <figcaption class="decker">
         
        This
         
        is
         
        a
         
        <strong>
            plain
        </strong>
         
        image.
    </figcaption>
</figure>
```

------------------------------------------------------------------------

``` {.markdown}
Inline ![Image URI with **query string**.](https:/some.where/image.png&key=value)
```

translates to

``` {.html}
<p>Inline <figure class="decker">
    <img class="decker" data-src="https:/some.where/image.png&key=value">
    <figcaption class="decker">
        Image
         
        URI
         
        with
         
        <strong>
            query
             
            string
        </strong>
        .
    </figcaption>
</figure>
</p>
```

------------------------------------------------------------------------

``` {.markdown}
Inline ![Image with **attributes**](/some/path/image.png){#myid .myclass width="40%" css:border="1px" myattribute="value"}
```

translates to

``` {.html}
<p>Inline <figure id="myid" class="decker myclass" data-myattribute="value" style="border:1px;width:40%;">
    <img class="decker" data-src="/some/path/image.png">
    <figcaption class="decker">
        Image
         
        with
         
        <strong>
            attributes
        </strong>
    </figcaption>
</figure>
</p>
```

------------------------------------------------------------------------

``` {.markdown}
Inline ![A local video.](/some/path/video.mp4){width="42%"}
```

translates to

``` {.html}
<p>Inline <figure class="decker" style="width:42%;">
    <video class="decker" data-src="/some/path/video.mp4#">
        
    </video>
    <figcaption class="decker">
        A
         
        local
         
        video.
    </figcaption>
</figure>
</p>
```

------------------------------------------------------------------------

``` {.markdown}
Inline ![A local video with start time.](/some/path/video.mp4){start="5" stop="30" preload="none"}
```

translates to

``` {.html}
<p>Inline <figure class="decker">
    <video class="decker" data-src="/some/path/video.mp4#t=5,30" preload="none">
        
    </video>
    <figcaption class="decker">
        A
         
        local
         
        video
         
        with
         
        start
         
        time.
    </figcaption>
</figure>
</p>
```

------------------------------------------------------------------------

``` {.markdown}
Inline ![A local video with all features on.](/some/path/video.mp4){.controls .autoplay start="5" stop="30" poster="somewhere/image.png" preload="none"}
```

translates to

``` {.html}
<p>Inline <figure class="decker">
    <video class="decker" data-src="/some/path/video.mp4#t=5,30" autoplay="1" controls="1" poster="somewhere/image.png" preload="none">
        
    </video>
    <figcaption class="decker">
        A
         
        local
         
        video
         
        with
         
        all
         
        features
         
        on.
    </figcaption>
</figure>
</p>
```

</div>
