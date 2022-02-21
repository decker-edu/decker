---
title: Accessibility Features
---

# Image Alternative Texts

# Specification {.sub}

|                                          | caption | `title=` | `aria-label=` |
|------------------------------------------|---------|----------|---------------|
| `[alt](url)`                             | `alt`   | `alt`    | `alt`         |
| `[alt](url "title")`                     | `alt`   | `title`  | `alt`         |
| `[alt](url "title"){aria-label="aria"}`  | `alt`   | `title`  | `aria`        |
| `[alt](url){aria-label="aria"}`          | `alt`   | `alt`    | `aria`        |
| `[](url "title")`                        |         | `title`  | `title`       |
| `[](url "title"){aria-label="aria"}`     |         | `title`  | `aria`        |
| `[](url){aria-label="aria"}`             |         |          | `aria`        |

# None Specified {.sub}

![](include/06-metal.png)

# Only Caption {.sub}

![Caption](include/06-metal.png)

# Only Title {.sub}

![](include/06-metal.png "Title")

# Only Label {.sub}

![](include/06-metal.png){aria-label="Label"}

# Caption Title {.sub}

![Caption](include/06-metal.png "Title")

# Caption Label {.sub}

![Caption](include/06-metal.png){aria-label="Label"}

# Title Label {.sub}

![](include/06-metal.png "Title"){aria-label="Label"}

# Everything {.sub}

![Caption](include/06-metal.png "Title"){aria-label="Label"}

# Error

If you can see this, Issue #344 has been resolved