document.addEventListener('DOMContentLoaded', function () {
    const menuItems = document.querySelectorAll('.menu-bar li span');

    menuItems.forEach(item => {
        item.addEventListener('click', function (e) {
            const parent = this.parentElement;

            // Toggle "open" class
            parent.classList.toggle('open');

            // Prevent the default action if it's a span
            if (e.target.tagName === 'SPAN') {
                e.preventDefault();
            }
        });
    });
});