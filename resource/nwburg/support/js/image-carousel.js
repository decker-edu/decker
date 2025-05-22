// Load the Slick carousel library and initialize the image carousel
    document.addEventListener("DOMContentLoaded", function () {
        $(".bannerSlider").slick({
            dots: true,
            infinite: true,
            speed: 300,
            slidesToShow: 1,
            adaptiveHeight: true,
            autoplay: true,
            autoplaySpeed: 4000, // Default speed, overwritten by YAML if provided
        });

        // Adjust autoplay speed for each slide
        $(".image-carousel").on("init", function (event, slick) {
            const slides = slick.$slides;
            slides.each(function (index, slide) {
                const duration = parseInt($(slide).data("duration")) || 4000;
                slick.slickSetOption("autoplaySpeed", duration, true);
            });
        });
    });

    // 	//banner slider
	// $(".bannerSlider").slick({
	//     dots: false
	//     , autoplay: true
	//     , infinite: true
	//     , dots: true
	//     , slidesToShow: 1
	//     , slideswToScroll: 1
	//     , arrows: false
	// });