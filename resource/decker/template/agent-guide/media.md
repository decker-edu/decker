# Media

All media use the Markdown image syntax. The URL and the classes decide what is
generated:

```markdown
![Caption](path/to/file.ext "Label for screen readers"){width="50%"}
```

| Media    | Extension or scheme                                | Force with     |
|----------|----------------------------------------------------|----------------|
| Image    | `jpg` `jpeg` `png` `gif` `tif` `tiff` `bmp` `svg`  | `.image`       |
| Video    | `mp4` `mov` `ogg` `avi` `webm`                     | `.video`       |
| Iframe   | `html` `htm` `php`                                 | `.iframe`      |
| PDF      | `pdf`                                              | `.pdf`         |
| 3D model | `off` `obj` `stl` `ply` `pmp` `xyz` `agi`          | `.mview`       |
| 3D model | `gltf` `glb`                                       | `.modelviewer` |
| GeoGebra | `ggb`                                              | `.geogebra`    |
| Audio    | `mp3` `aiff` `wav`                                 | `.audio`       |
| Stream   | `youtube:ID` `vimeo:ID` `twitch:ID`                |                |

**Gotchas:**

- A URL without a known extension is an **image**. A web page needs the class:
  `![](https://example.com/){.iframe}`.
- A missing local file is a **hard** error. A remote URL is not checked.
- Only the file itself is copied to `public/`. Everything it loads (the images of an
  iframe page, the imports of a module) must be listed in `static-resources` in
  `decker.yaml` or located in one of the `static-resource-dirs`.
- Audio without `.controls` shows nothing. A video without `.controls` or `.autoplay`
  cannot be started.
- `w:h="4/3"` only sets the shape of iframes, PDFs, 3D models, GeoGebra and streams,
  and of any medium in a row. `align="right"` only works on images.

## Images and captions

The caption comes from the square brackets, or from a paragraph directly below that
starts with `Caption:`. An image among other text is part of the line and needs a
`height`.

````markdown {.decker-example file="images-deck.md" project="images"}
# Images

![A figure](dot.svg){width="30%"}

An icon ![](dot.svg){height="1em"} in the text.

![](dot.svg){.embed width="20%"}

Caption: Embedded into the HTML, so that its elements can be styled.
````

```xml {.decker-example file="dot.svg" project="images"}
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 10 10"><circle cx="5" cy="5" r="4"/></svg>
```

A line block makes a row, with the same height for all media:

```markdown {.decker-example file="row-deck.md" project="images"}
# Row

| ![One](dot.svg)
| ![Two](dot.svg){w:h="2/1"}

Caption: Two images.
```

A missing file fails the build:

```markdown {.decker-example file="missing-deck.md" expect="error"}
# Missing

![](does-not-exist.png)
```

## Slide backgrounds

The last image in a level-1 heading becomes the background. It takes `size`,
`position`, `repeat` and `opacity`. A video takes `size`, `opacity`, `.loop` and
`.muted`, an iframe or PDF `.interactive`. In handouts and pages, the medium appears
below the heading.

A color goes on the heading, not on the image.

```markdown {.decker-example file="background-deck.md" project="images"}
# Picture ![](dot.svg){size="contain" opacity="0.5"}

# Color {color="red"}
```

## Other media

```markdown {.decker-example file="others-deck.md" project="others"}
# Others

![A page](frame.html?x=1){w:h="2/1" width="50%"}

![A video](movie.mp4){.controls .autoplay .loop .muted start="5" stop="30"}

![A sound](sound.mp3){.controls}

![A document](manual.pdf){height="500px"}

![A model](model.off){width="600px" height="400px"}

![An applet](applet.ggb){width="600px" height="400px" showToolBar="true"}

![On YouTube](youtube:TbppXMGrsvw){.autoplay start="750" width="60%"}
```

```html {.decker-example file="frame.html" project="others"}
<!doctype html>
<title>A page</title>
```

```text {.decker-example file="movie.mp4" project="others"}
Stands in for a video here.
```

```text {.decker-example file="sound.mp3" project="others"}
Stands in for audio here.
```

```text {.decker-example file="manual.pdf" project="others"}
Stands in for a PDF here.
```

```text {.decker-example file="model.off" project="others"}
Stands in for a 3D model here.
```

```text {.decker-example file="applet.ggb" project="others"}
Stands in for a GeoGebra file here.
```

The other attributes of an iframe appear on it as `data-*`. A video stream also takes
the player parameters of the service as attributes (`controls="0"`).

## Source code from a file

`.code` includes a file, another class names the language. A fragment selects a part
between `---8<--- name` and `--->8---`, or from `---8<|--- name` up to the next empty
line. **An unknown name gives an empty block, without a diagnostic.**

```markdown {.decker-example file="code-deck.md" project="code"}
# Code

![The whole file](code.py){.code .python}

![Only a part](code.py#setup){.code .python}
```

```python {.decker-example file="code.py" project="code"}
# ---8<--- setup
def setup():
    return 1
# --->8---
```

## Diagrams

A code block with the class of a language and `.render` runs a program and shows the
SVG as an image. A file works with `.render` alone (`![](graph.dot){.render}`).

| Class       | File        | Program                  |
|-------------|-------------|--------------------------|
| `.dot`      | `.dot`      | `dot`                    |
| `.gnuplot`  | `.gnuplot`  | `gnuplot`                |
| `.plantuml` | `.plantuml` | `plantuml`               |
| `.mermaid`  | `.mmd`      | `mmdc`                   |
| `.d2`       | `.d2`       | `d2`                     |
| `.tex`      | `.tex`      | `pdflatex` and `pdf2svg` |

````markdown {.decker-example file="graph-deck.md" requires="dot"}
# A graph

``` {.dot .render width="40%"}
digraph { A -> B }
```

Caption: Rendered by Graphviz.
````

**Gotchas:**

- **A failing program does not fail the build, not even with `--strict`.** The exit
  code stays 0. Decker prints `# ERROR:` and generates an image with the message in
  red. After every build with a diagram, look for `# ERROR:` in the output, or at the
  rendered slide.
- If you install a program after such an error, run `decker clean`, or the image
  of the error stays.
- `decker check` lists the programs it can find. They are not part of Decker. Do not
  use a diagram whose program is missing without telling the user.
- A diagram fills the width of its block, so a small graph can get very big and leave
  the slide. Set a `width` or a `height`.

## Charts

A code block with one of the classes `.line-chart`, `.bar-chart`, `.stackedBar-chart`,
`.horizontalBar-chart`, `.radar-chart`, `.doughnut-chart`, `.pie-chart` or
`.polarArea-chart`. The first line holds the labels, each further line starts with
the name of a data set. It takes `width`, `height`, `title`, `.fill`, `.nofill` and `.log`.

**Charts only work in decks.** A handout or a page shows the block as text.

````markdown {.decker-example file="chart-deck.md"}
# Sales

``` {.bar-chart width="60%" title="Sales per month"}
January, February, March
Apples, 65, 59, 80
Pears, 28, 48, 40
```
````

## Running Javascript

A code block with `.javascript` and `.run` runs as an ES6 module. `anchor` is the empty
element at its position. A `.js` file with `.run` works too, and gets the element with
`Decker.anchor(import.meta)`.

````markdown {.decker-example file="module-deck.md"}
# A module

``` {.javascript .run height="100px"}
anchor.innerText = "Hello";
```
````

See the [users' guide](https://decker-edu.github.io/decker/users-guide/decker-users-guide.html), section *Media handling*, for all
attributes.
