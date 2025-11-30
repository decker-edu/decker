$(document).ready(function () {
    $('.your-slider').slick({
        dots: true,             // Show dots navigation
        infinite: true,         // Loop through the slides
        speed: 300,             // Slide transition speed in milliseconds
        slidesToShow: 1,        // Number of slides to show at once
        adaptiveHeight: true    // Adjusts height based on the content of the slide
    });
});