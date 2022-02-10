This is a first attempt at a port of the old decker default CSS. As this was
mainly done by Marc and is in its unmodified form mainly used in Würzburg the
resource pack is named `wburg`. To use it just add

``` yaml
resource-pack: exe:wburg
```

to `decker.yaml`.

Stuff in the resource pack `wburg`:

    resource/wburg
    ├── example/
    ├── readme.md
    ├── support
    │   ├── css
    │   │   ├── fonts.css
    │   │   ├── light.min.css
    │   │   └── wburg.css
    │   └── fonts
    │       ├── roboto/
    │       ├── roboto.css
    │       ├── source-code-pro/
    │       └── source-code-pro.css
    └── template
        ├── deck-title.html
        ├── default.yaml

CSS mainly comes from the original `resource/decker/css` and is augmented by
`wburg.css`.

The templates are the originals from `resource/decker/template` with the
exception of `deck-title.html` which has some specific modifications.

Things that seem to work:

-   Fonts selection (Roboto and Source Code Pro)
-   Title page
-   Embedded media in block contexts
-   Columns
-   Margin bar highlights for blocks
-   `.align-*` class annotations
-   CSS variable transfer from YAML meta data

Things that are known not to work (yet):

-   Streaming video in inline contexts shrinks the iframe to 0, 0
-   Successive media blocks in block contexts do not show up side by side (wont
    fix)
-   Exact margin spacing for assorted elements

Things that are still missing:

-   Colors from the shades and accents palette
-   Probably a few more things
