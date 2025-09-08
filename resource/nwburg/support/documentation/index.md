# Documentation for index.yaml

This documentation provides an overview of the structure and syntax of the index.yaml file used to generate the index page. It describes each section of the YAML file, its fields, and the rules to be followed when creating or editing the file.

- [Documentation for index.yaml](#documentation-for-indexyaml)
  - [Overview](#overview)
  - [Structure](#structure)
    - [Menu Bar](#menu-bar)
    - [Image Carousel](#image-carousel)
    - [Chapters](#chapters)
  - [Field Descriptions](#field-descriptions)
  - [Examples](#examples)
  - [Appendix](#appendix)
    

## Overview

The index.yaml file contains three main components:
	1.	Menu Bar: Defines the navigation structure of the index page.
	2.	Image Carousel: Displays rotating images with captions.
	3.	Chapters: Lists the lectures and associated materials, organized by sections.

Each component is hierarchical, with nested items and fields.

## Structure

### Menu Bar

The menu_bar section contains navigation items and optional submenus.

```markdown
menu_bar:
  items:
    - label: "Home"
      action: "/"
      style: "menu-item"
```

Key Fields:

- label (required): Display text for the menu item.
- action (optional): URL or path for the action when clicked. Use "mailto:" for email links.
- style (optional): CSS class or style identifier.
- submenu (optional): Nested menu items, structured the same as the top-level items.

### Image Carousel

The image_carousel section defines a rotating image banner.
```markdown
image_carousel:
  items:
    - image: "/img/example.png"
      caption: "Example Caption"
      duration: 5
      style: "carousel-item"
```

Key Fields:

- image (required): Path to the image file.
- caption (optional): Text displayed with the image.
- duration (optional): Time (in seconds) the image is displayed.
- style (optional): CSS class or style identifier.

### Chapters

The chapters section lists the lectures, their dates, and their materials.
```markdown

chapters:
  - title: "Lecture 01: Example Topic"
    date: "2024-01-01"
    sections:
      - title: "Introduction"
        materials:
          - title: "Slides"
            files:
              - Name: "Slide Deck"
                Path: "slides.html"
                Icon: "/icons/slide-icon.svg"
                IsReady: true
            keywords: ["slides", "example"]
```

Key Fields:

- title (required): Title of the lecture or section.
- date (required): Date of the lecture in YYYY-MM-DD format.
- sections (required): Subsections of the lecture. Each section has:
  - title: Title of the section.
  - materials: List of materials.
    - title: Title of the material.
    - files: List of file details.
      - Name: File name.
      - Path: File path.
      - Icon: Path to the icon representing the file type.
      - IsReady: Boolean indicating if the file is available.
      - keywords: Tags associated with the material.

## Field Descriptions

Required vs Optional Fields

| Field            | Required | Description                                 |
| :--------------- | :------- | :------------------------------------------ |
| label            | Yes      | Text displayed to the user.                 |
| action           | No       | URL or path executed on click.              |
| style            | No       | Defines the CSS class or style.             |
| submenu          | No       | Nested items for dropdown menus.            |
| image            | Yes      | Path to the image file.                     |
| caption          | No       | Text shown with the image.                  |
| duration         | No       | Display time for the image in seconds.      |
| title            | Yes      | Title of the lecture, section, or material. |
| date             | Yes      | Date in YYYY-MM-DD format.                  |
| materials        | No       | List of teaching materials.                 |
| files            | No       | List of file details for a material.        |
| keywords         | No       | List of tags to help categorize materials.  |
| Name, Path, etc. | Yes      | Fields within files are mandatory.          |

Style Rules

- Menu Bar: Use menu-item for primary items and submenu-item for dropdown items.
- Image Carousel: Use carousel-item for consistent styling.
- Chapters: Apply specific styles for icons (e.g., /icons/slide-icon.svg).

Field Parsing

- Strings must be enclosed in quotes if they contain special characters (e.g., mailto:, URLs).
- Boolean fields (e.g., IsReady) accept true or false values.
- Dates follow the ISO 8601 format (YYYY-MM-DD).

## Examples

Example 1: Simple Menu Item
```markdown

- label: "Home"
  action: "/"
  style: "menu-item"
```

Example 2: Chapter Section with Multiple Materials
```markdown

- title: "Lecture 01: Example Topic"
  date: "2024-01-01"
  sections:
    - title: "Introduction"
      materials:
        - title: "Slides"
          files:
            - Name: "Slide Deck"
              Path: "slides.html"
              Icon: "/icons/slide-icon.svg"
              IsReady: true
          keywords: ["slides", "introduction"]
```

## Appendix

- Supported Actions:
  - mailto: for email links.
  - Relative paths (/example) or absolute URLs (https://example.com).
- Styling Guidelines:
  - Use predefined styles for consistent appearance.
  - Define new styles in the associated CSS file if needed.