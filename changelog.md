# Revision History for Decker

## decker 0.14.0

-   Move `rsync` configuration entirely to `decker.yaml`

    This will certainly break projects that define their own rsync options. They
    will have to add the moved rsync options:

    ``` yaml
    publish:
    rsync:
        options:
        - "--recursive"
        - "--copy-links"
        - "--delete"
        ...
    ```

## decker 0.13.0

-   Use OKSolar (https://meat.io/oksolar) as default palette for resource pack
    `mono`.

-   Add syntax extensions from Kate (https://kate-editor.org/syntax/) for
    built-in code highlighting by through meta variable
    `extra-highlight-syntax`. For example:

    ``` yaml
    extra-highlight-syntax:
      Asm6502: asm6502.xml
    ```

-   Better messages for errors during template compilation.

-   Save whiteboard annotation when recording stops (`zzz`) and also save them
    to the local download folder.

-   Embed Geogebra (https://www.geogebra.org) apps using standard image
    notation. For Example:

    ``` markdown
    ![Interactive Geogebra](gradient.ggb)
    ```

-   Automatically open recording panel after initiating recording mode ('r').

-   Defer device search until after initiation of recording mode ('r').

-   Block quotes can now have captions and are layouted as a standard media
    element. For example:

    ``` markdown
    > The answer is 42.

    Caption: Douglas Adams, *The Hitchhikers Guide to the Galaxy*
    ```

-   Add the new `nwburg` resource pack containing the JMU templates and styles.

-   Various fixes and enhancements to the `tudo` resource pack.

-   Handle palettes of arbitrary size. The lower half of the palette is treated
    as shades, the upper half as accents.

-   Removed files (like `*-annot.json`) are now also removed from `public`.

-   Add `decker purge` command. There two cleanup commands now:

    -   `decker clean` - removes `public`.
    -   `decker purge` - also removes `.decker` (which contains crunched MP4s)

-   Rename: `decker-engine` is now `feedback`.

-   Handouts are not built by default anymore. Use `decker handouts` explicitly.

-   Additional math macros have been removed from the default resource pack. Use
    your favorite resource pack or `decker.yaml` to define them.

-   Decks that have `draft: true` or whose deck ids are the `no-index:` list are
    removed from the search index.

-   Use GPL-3.0 license.

-   Add the `tudo` resource pack.

-   Add template macros. For example:

    ``` yaml
    assign: |
      <a class="index-name" title="HTML Seite" href="assignments/:(url)-page.html">:(title)
      <i class="fas fa-file-code"></i></a>
    ```

    can be invoked as

    ``` markdown
    [@assign](a05 "Aufgabe 5")
    ```

    and yields

    ``` markdown
    <a class="index-name" title="HTML Seite" href="assignments/a05-page.html">Aufgabe 5<i class="fas fa-file-code"></i></a>
    ```

-   Add dynamic Lua filters for the Pandoc AST. For example:

``` yaml
pandoc:
  filters:
    before:
    - upcase-h1.lua
```

with `upcase-h1.lua` containing:

``` lua
local text = pandoc.text

function Header(el)
    if el.level == 1 then
      return el:walk {
        Str = function(el)
            return pandoc.Str(text.upper(el.text))
        end
      }
    end
end
```

transforms:

``` markdown
# A Lua Plugin {.columns}
```

to:

``` markdown
# A LUA PLUGIN {.columns}
```

-   Use *Scotty* instead of *Snap* as a webserver.

-   Add a separate `template/index.html` template and `support/css/index.css`
    for the index page.

-   Render blocks and DIVs with class `details` as
    `<detail><summary> ... </summary> ... </detail>`

-   Whiteboard uses all 16 palette colors in light and dark mode. The whiteboard
    button is only shown in presenter mode.

-   There is a presenter mode now (toggle with `ppp`). Currently enables the
    following:

    -   Arm the recording key (\`\`).
    -   Show the whiteboard button.
    -   Start a quiz session with the quiz server, if configured.

-   Panndocs footnotes are now rendered at the end of the slide, wrapped in a
    DIV with class `footnotes`.

-   Many, many bugs have been fixed all around.
