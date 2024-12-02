# Revision History for Decker

## decker 0.14.0

- Palette for charts can now be defined in YAML in `chart.colors`. 
  For this to work, disable the color plugin of Chart.js.
  ``` yaml
  chart:
    colors:
      - #ff0000
      - #00ff00
      - #00ff00
    defaults:
      plugins:
        colors:
          enabled: false
  ```

- The colorscheme ("light" or "dark") can now be enforced in YAML. Simply add
    ``` yaml
    colorscheme: "light"
    ```
    or `colorscheme: "dark"` to your YAML settings.

-   The feedback menu now has a "send" button.
-   The admin login button in the feedback menu is now clearly visible and also has a button to send the login request.

- Add a legend to the feedback menu that is only visible on mobile devices (small screens).
-   Decks can now be accessed in Presenter, Handout and A11Y Mode directly
    by appending ?presenter, ?handout or ?a11y to the deck's url.

-   The index page can now insert direct links to Presenter, Handout or A11Y
    Mode.
    Which of these is displayed can be configured with:

    ``` yaml
    index:
      progress: true
      links:
        - a11y
        - handout
        - presenter
    ```

-   Handout Mode now supports zooming both by the (-)/(+) buttons in the
    top right corner and the browser's native zoom level. In addition,
    if the slides are zoomed away from, they center while if they are
    zoomed in they attach to the left side of the screen to properly
    allow scrolling the page. (Previously they fell out of the left side
    of the viewport).

-   All modes (presenter, handout, a11y) as well as functions accessible
    only by keyboard inputs (recorder) are now accessible through buttons
    in the main navigation menu. This solves the issue of some lecture
    halls not providing a keyboard.

-   The settings menu has been removed and replaced by a single
    light / dark mode switch.

-   Zooming in/out of a slide element is by default triggered via 
    double-click (left mouse button). It can now be configured to react on 
    double-click, triple-click, or alt-click (alt-key and left mouse).

    ``` yaml
    zoom:
      trigger: { "doubleClick" | "tripleClick" | "altClick" }
    ```

-   Decker slides can now be installed as progressive web apps (PWAs). We don't
    cache the content locally, so consumers of published slides still need
    internet connection. But the PWA mechanism allows to add slides to the
    home-screen (for iOS) or as an app shortcut (Chrome, Safari).

-   The whiteboard can now be configured to automatically switch on (when a
    pen-hover is detected) and off (two seconds after pen-up event). While it is
    convenient not having to toggle the whiteboard manually, it also means that
    one cannot use a pen to interact with elements on the slide. By default this
    feature is disabled. To enable, use this setting:

    ``` yaml
    whiteboard:
      autotoggle: true
    ```

-   Remove inert polyfill, since the inert feature is now supported in
    up-to-date browsers.

-   `column-1-2-3` DIV style columns are now detected recursively.

-   Files can be excluded from file watching using glob patterns:

    ``` yaml
    watch:
        exclude: 
            - "**/*.swp"
            - "**/~*"
    ```

-   The new command `decker transcribe` transcribes all recorded videos to
    `.vtt` files that are available with the integrated video player. The
    recorded language must be configured explicitly, default is German. By
    default, both German and English subtitles will be generated. In order to
    use this, install [whisper.cpp](https://github.com/ggerganov/whisper.cpp).
    Those are the configuration options.

    ``` yaml
    # whisper.cpp default transcription settings
    whisper:
      base-dir: /usr/local/share/whisper.cpp
      model: models/ggml-large.bin
      # the recorded language (e.g., "de" or "en")
      lang: de 
    ```

-   Move `rsync` configuration entirely to `decker.yaml`.

    This will certainly break projects that define their own rsync options. They
    will have to add the removed options to their local rsync options list:

    ``` yaml
    publish:
    rsync:
        options:
        - "--recursive"
        - "--copy-links"
        - "--delete"
        ...
    ```

-   The title attribute on links and images is used as the `aria-label`, there
    are no more tooltips on links and images.

-   The title slide can now be extensively customized with multiple authors,
    institutions and logos. For example:

    ``` yaml
    authors:
    - name: 'First Author'
        url: 'https://example.com/first'
        affiliation: # 'First Department'
        name: 'First Department'
        url: 'https://example.com/first-dep'
        logo: '/test/decks/assets/dummy-long.png'
    - name: 'Second Author'
        url: 'https://example.com/second'
        affiliation: 'Second Department'
    - 'Third Author'
    ```

-   The use of `highlight.js` for code syntax highlighting is no longer
    supported. Use Pandoc highlighting by setting `highlight-style:` to
    something. To see supported highlight styles do

    ``` sh
    pandoc --list-highlight-styles
    ```

-   3D *glTF* files (https://www.khronos.org/gltf/) can now be embedded using
    Googles *model-viewer* (https://modelviewer.dev/):

    ``` md
    ![Expressive Robot (with autoplay)](assets/robot.glb){autoplay=1}
    ```

-   Decks can be switched to *handout mode* during presentation. All slides will
    be shown in one linearly scrollable HTML document view. Special handout
    versions of decks are no longer created by default.

-   Static resources can now be listed as file or directory paths in the
    variable `static-resources`. Directories are traversed recursively, adding
    all contained files to the list.

-   *Examiner* style poll (the ones loaded from YAML files) results can now be
    saved if running off a local Decker server (`decker -S`). They are stored in
    `*-poll.json` files alongside the presentation sources. Enable with:

    ``` yaml
    save-polls: true
    ```

-   Always generate an index document. If a custom `index.md` is present, the
    generated index is stored as `generated-index.html` alongside the custom
    `index.html`.

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
