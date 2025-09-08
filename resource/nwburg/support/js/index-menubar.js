document.addEventListener('DOMContentLoaded', function () {
    const hamburger = document.querySelector('.hamburger');
    const navMenu = document.getElementById('nav-menu');

    // Toggle main menu
    hamburger.addEventListener('click', function () {
        navMenu.classList.toggle('active');
    });

    // Toggle submenu items
    document.querySelectorAll('.submenu-toggle').forEach(toggle => {
        toggle.addEventListener('click', function () {
            const parentLi = this.closest('li');
            const submenu = parentLi.querySelector('.submenu');

            if (parentLi && submenu) {
                const isOpen = parentLi.classList.toggle('open');
                submenu.style.display = isOpen ? 'block' : 'none';
            }
        });
    });
});
