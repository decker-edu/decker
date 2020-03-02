Todo:
- alt text (z.B. "Marc")

------------------------------------------------

![my caption](bla.pdf){ .myclass width=xx height=yy foo=bar }

<figure style="width:xx; height:yy; foo:bar;" class="myclass">
    <object type="application/pdf" data="bla.pdf"></object>
    <figcaption> my caption </figcaption>
</figure>

------------------------------------------------

![](bla.pdf){ .myclass width=xx height=yy foo=bar }

<object type="application/pdf" data="bla.pdf" style="width:xx; height:yy; foo:bar;" class="myclass"></object>

------------------------------------------------

![my caption](image.jpg){ .myclass width=xx height=yy foo=bar }

<figure style="width:xx; height:yy; foo:bar;" class="myclass">
    <img src="image.jpg">
    <figcaption> my caption </figcaption>
</figure>

------------------------------------------------

![](image.jpg){ .myclass width=xx height=yy foo=bar }

<img src="image.jpg" class="myclass" style="width:xx; height:yy; foo:bar;">

------------------------------------------------

![](image.svg){ .embed .myclass width=xx height=yy foo=bar }

<svg allesganzschlimmembedden>

------------------------------------------------

![my caption](video.mp4){ .myclass .autoplay .controls .loop .muted width=xx foo=bar start=5 stop=10}

<figure style="width:xx; foo:bar;" class="myclass">
    <video data-src="video.mp4#t=5,10" controls=1 loop=1 muted=1 autoplay=1>
    <figcaption> my caption </figcaption>
</figure>

------------------------------------------------

![](video.mp4){ .myclass .controls .autoplay .loop .muted width=xx foo=bar start=5 stop=10}

<video style="width:xx; foo:bar;" class="myclass" data-src="video.mp4#t=5,10" controls=1 loop=1 muted=1 autoplay=1>

------------------------------------------------

![my caption](demo.html){ .myclass data-model=bla width=xx height=yy foo=bar [.iframe] }

<figure style="width:xx; height:yy; foo:bar;" class="myclass">
    <iframe data-src="demo.html" allow="fullscreen" data-model=bla>
    <figcaption> my caption </figcaption>
</figure>

------------------------------------------------

![](demo.html){ .myclass data-model=bla width=xx height=yy foo=bar [.iframe] }

<iframe data-src="demo.html" allow="fullscreen" data-model=bla style="width:xx; height:yy; foo:bar;" class="myclass">

------------------------------------------------

![my caption](model.off){ .myclass data-texture=bla.jpg width=xx height=yy foo=bar }

<figure style="width:xx; height:yy; foo:bar;" class="myclass">
    <iframe data-src="support/vendor/mview/mview.html" allow="fullscreen" data-model=model.off data-texture=bla.jpg>
    <figcaption> my caption </figcaption>
</figure>

------------------------------------------------

![](bla.dot){ .render .myclass width=xx height=yy foo=bar }

<img src="bla.svg" style="width:xx; height:yy; foo:bar;" class="myclass">

------------------------------------------------

![caption](bla.cpp){ .code .myclass [start=5] [stop=10] [snippet="constructor"] width=xx height=yy foo=bar }

<figure style="width:xx; height:yy; foo:bar;" class="myclass">
    <pre><code>
         selected content of bla.cpp
    </code></pre>
    <figcaption> my caption </figcaption>
</figure>


