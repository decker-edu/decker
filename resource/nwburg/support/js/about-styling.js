"use strict";

// Toggle lecture content visibility on title click
document.addEventListener("DOMContentLoaded", async function () {
    try {

        // Add event listeners for toggling lecture content visibility
        const lectureHeaders = document.querySelectorAll(".lecture-header");

        lectureHeaders.forEach(lh => {
            lh.addEventListener("click", function () {
                const lectureContainer = this.nextElementSibling; 
                
                if (lectureContainer) {
                    lectureContainer.style.display = (lectureContainer.style.display === "none" || lectureContainer.style.display === "") ? "block" : "none";
                }
            });
        });

        // Initially hide all lecture content sections
        document.querySelectorAll(".lecture-container").forEach(container => {
            container.style.display = "block";
        });
    } catch (error) {
        console.error("Error in main execution:", error);
    }
});