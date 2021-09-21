---
date: September 17th 2021
title: Decker Dev Day
reveal:
  slideNumber: true
  vertical: true
highlightjs: atom-one-light
---

# Agenda

1.  Branch `develop`
2.  Winter 2021
3.  Development model $\infty$

# Branch `develop`

# Branch `develop` {.sub}

## What is happening there? {.fragment}

1.  Upgrades
2.  Resource Packs
3.  HTML + CSS structure cleanup

# Upgrades {.sub}

## Pandoc 2.14.2 {.fragment}

-   Nothing important for the end-user
-   Some Haskell API changes
-   More emojis 😄

## Reveal 4 {.fragment}

-   Plugins are now ES6 modules
-   So are our own plugins

# Cleaner templates {.sub}

## Decker meta data

-   Injected into the DOM wholesale

-   Global variable

    ``` js
    Decker.meta = { "title": "Really", "subtitle": "Cool!" };
    ```

## Reveal configuration {.fragment}

-   Now lives under the `reveal` key, i.e.

    ``` yaml
    reveal:
      controls: true
      hash: true
      history: false
    ```

-   No more implicit `true`, `false`, `0`, `1` conversion confusion

# Resource packs

# Resource packs

## Decker base pack

-   Contains default files for

    -   Templates
    -   Resources (CSS, Javascript, Fonts, Plugins, anything)
    -   Meta data

## Resource pack {.fragment}

-   Can contain the same things
-   Augments the base pack
-   Files in the base pack are replaced
    -   Except for `default.yaml`, which is merged

# Built-in resource packs

## *decker*

-   The new default, always used
-   Intentionally minimal
-   Focus on function, not form
-   Totally unfinished

## *wburg*

-   The old default without all the *mario* stuff

## *mario*

-   The old *mario* stuff

# Resource pack selection

## Three possible sources

1.  From the Decker executable (built-in)
2.  From the file system (directory)
3.  From a ZIP-Archive (file)

## Selection in `decker.yaml` {.fragment}

``` yaml
# Valid resource packs in the decker executable
resource-pack: exe:wburg
# resource-pack: exe:mario
```

# Development on resource packs {.columns}

## Location in the repo {.left}

    resource
    ├── .DS_Store
    ├── decker
    │   ├── support
    │   └── template
    ├── mario
    │   ├── support
    │   └── template
    └── wburg
        ├── .DS_Store
        ├── example
        ├── support
        └── template

## Development {.right}

Use:

``` sh
stack run -- decker -s
```

Because:

1.  Reads built-in resources from file system
2.  Tracks dependencies
3.  Reloads the browser

# HTML + CSS structure cleanup

# HTML + CSS structure cleanup

## Slide structure

-   One structure for all current and future layout options
-   Handle vertical reveal slides

## Media structure

-   One structure for all embedded media objects
-   Works in *block* and *inline* contexts (mostly)

# Slide structure

## HTML

``` html
<section id="a-slide" id="a-slide" class="slide level1 present">
  <div class="decker">
    <div class="alignment">
      <h1>Licht</h1>
      <div class="layout">
        <div class="area">
          <div class="block">
            <h2>Strahlung und so</h2>
            <p>Something or other</p>
          </div>
        </div>
        <div class="area">
          <div class="block">
            <p>No header here</p>
            <p>Something or other</p>
          </div>
          <div class="block">
            <h2>Sichtbares Licht</h2>
            <p>Something or other</p>
          </div>
        </div>
      </div>
    </div>
  </div>
</section>
```

Caption: HTML structure of a slide. [Watch in action](static/slide-layout.html).

# Media structure

## Markdown

``` markdown
![A plain local image](static/06-metal.png)
```

## HTML {.fragment}

``` html
<div class="media">
  <figure class="image">
    <img src="static/06-metal.png" alt="06-metal.png" />
    <figcaption>
      A plain local image
    </figcaption>
  </figure>
</div>
```

# Media structure

## Markdown

```` markdown
``` markdown
![](static/06-metal.png)
```
````

## HTML {.fragment}

``` {.html}
<div class="markdown media">
  <figure class="code">
    <pre><code>![](static/06-metal.png)</code></pre>
  </figure>
</div>
```

# All Media are equal {.columns}

## Code {.left}

``` html
<div class="markdown media">
  <figure class="code">
    <pre><code>![](static/06-metal.png)</code></pre>
  </figure>
</div>
```

Caption: Some code.

## Streams {.right}

![Some fake.](youtube:7XX6IEuLP3A){aspect="16:9"}

## Even inline {.bottom}

This ![Some balls.](static/06-metal.png){height="120px"} is
![](../test/decks/pacman-perfect-game.mp4){.controls .mute height="151px"}
inline ![Some balls.](static/06-metal.png){height="120px"}

# Inline Javascript execution {.columns}

## Markdown {.top}

    ![Some Javascript.](../test/static/es6-blue.js){.javascript .run}

## Javascript {.left}

![](../test/static/es6-blue.js){.javascript .code}

## Result {.right}

![Some Javascript.](../test/static/es6-blue.js){.javascript .run}

# Winter 2021

# Winter 2021

## TODO

1.  Backport index search
2.  Fix outstanding bugs and issues
3.  Finish audience response quizzes
4.  Make a new release (0.11.0)

# Development model

# Development model

## Topics

-   Chat channel
    - [Matrix](https://matrix.org)

# Branches

## `master` {.fragment}

-   Protected, only Henrik can merge
-   Production
-   Never breaks

## `develop` {.fragment}

-   All developers have commit rights
-   Development and experiments
-   Should not break

## Feature branches {.fragment}

-   Against `master` if for production
-   Against `develop` if experimental

