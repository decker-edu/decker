document.addEventListener('DOMContentLoaded', function () {
    const hamburger = document.querySelector('.hamburger');
    const navMenu = document.getElementById('nav-menu');
    const submenuToggles = document.querySelectorAll('.submenu-toggle');

    // Toggle mobile menu
    hamburger.addEventListener('click', function () {
        navMenu.classList.toggle('active');
    });

    // Toggle submenu items
    submenuToggles.forEach(toggle => {
        toggle.addEventListener('click', function () {
            const parent = this.parentElement;
            parent.classList.toggle('open');

            // Toggle submenu visibility
            const submenu = parent.querySelector('.submenu');
            if (submenu) {
                submenu.style.display = submenu.style.display === 'block' ? 'none' : 'block';
            }
        });
    });
});
