# Decker Engine

## How stupid can a name be?

-   Amazing what lack of imagination can do to a perfectly fine webapp

## Any other questions?

-   Just type them into the text area and submit them with ⇧⏎
    (Shift-Return)
-   If you want to be able to delete a question later, provide a user
    token

# TODO (1) {.x-small}

## Frontend

- [ ] Allow up-voting for questions
-   [ ] Ask server for token at startup
    -   [ ] Pass Basic Auth headers to XHR call with
        `XHR.withCredentials = true`
    - Offer to delete token
    - Do not allow custom tokens but still offer to store 
-   [ ] Slide the question pane in and out of view
    -   [x] Add question mark to slide
    -   [x] Add close button to pane
    -   [x] Animate sliding
    -   [ ] Disable the key binding for `?`
-   [x] Remove the background colors
-   [x] Find just the right level of transparency
-   [ ] Offer to generate the user token from HTML basic authentication
    headers if available
-   [x] Offer to store user token in local storage
-   [ ] Provide an API to an authenticated lecturer
    -   [ ] Generate overviews (like *all questions for a deck sorted by
        slide*)
    -   [ ] Allow to delete any question

# TODO (2)

## Backend

-   [ ] Create token endpoint using Basic Auth headers
-   [ ] Improve the API
    -   [x] Remove internal ids and tokens from the endpoint URLs
-   [ ] Battle test the server
    -   [ ] Large videos
    -   [ ] Hundreds of simultaneous connections
    -   [ ] Malicious attacks

# Decker Engine Backstage Area {.inverse background="black"}

## Test page

-   [test.html](https://tramberend.beuth-hochschule.de/decker/test.html)

## API Doc

-   [doc.md](https://tramberend.beuth-hochschule.de/decker/doc.md)

