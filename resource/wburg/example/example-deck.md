---
title: Decker Slide Tool Reference Guide
subtitle: Examples and Explanations
abstract: This slideset is used to test changes
affiliation: Chair for Human-Computer Interaction
author: Marc Erich Latoschik
date: 10.2.2022
template:
  title-page:
    banner: img/title-banner-jmu.jpg
    affiliation-logo: img/title-logo-hci.png
    teaser-img: img/title-teaser-icg.png
bibliography: example.bib
chalkboard: 'example-deck.json'
csl: 'chicago-author-date.csl'
---

# Navigation

Navigate this presentation with the controls in the bottom-right corner,
your arrow keys or the space bar.

Some explanations have examples on a separate slide. These will be
arranged below the respective slide and will be indicated by a down
arrow in the controls. Use the down arrow key to see them. If you use
the space bar to go through the presentation, the examples will
automatically follow their explanation.

The `<i class="fas fa-bars">`{=html}`</i>`{=html} icon in the
bottom-left corner opens a menu showing a table of contents of all
slides.

# Markdown Syntax {#syntax}

The Decker Slide Tool assists you in creating media-rich presentations
with a few easy to use Markdown commands. This user guide will highlight
some of the main styling features of Decker and provide examples on how
to use each feature.

Visit <http://pandoc.org> for additional information on Pandoc-Markdown
text formatting.

# New Slides {#slides}

Heading 1 (h1) headers create new slides.

##  {.split}

``` {.markdown}
# Heading 1 (h1) new slide
## Heading 2 (h2)
### Heading 3 (h3)
#### Heading 4 (h4)
```

## Heading 2 (h2)

### Heading 3 (h3)

#### Heading 4 (h4)

# Multicolumn Slides {#multicolumn}

``` {.markdown}
# Würzburg Sehenswürdigkeiten {layout="columns"}

## Die Residenz {.left}
Die Würzburger Residenz ist das Hauptwerk des ...

## Alte Mainbrücke {.center}
Diese erste Steinbrücke Deutschlands soll bereits um ...

## Dom St. Kilian {.right}
Ein Hauptwerk der deutschen Baukunst zur Zeit der ...
```

# Multicolumn example {#example-multicolumn layout="columns"}

## Die Residenz {.left}

Die Würzburger Residenz ist das Hauptwerk des süddeutschen Barock.

## Alte Mainbrücke {.center}

Die erste Steinbrücke Deutschlands soll bereits um 1120 errichtet worden
sein.

## Dom St. Kilian {.right}

Ein Hauptwerk der deutschen Baukunst und viertgrößte romanische Kirche
Deutschlands.

# Top and Bottom {#topBottom}

Additionally use the `.top` and `.bottom` tags can be used.

``` {.markdown}
# Top and Bottom Example {layout="columns"}

## Top Colum {.top}
First/top column spans across the following columns.

## Left Column {.left}

## Right Column {.right}

## Third Column {.bottom}
Third/bottom column spans across the columns above.
```

# Top and Bottom Example {#example-topBottom layout="columns"}

## Top Colum {.top}

First/top column spans across the following columns.

## Left Column {.left}

## Right Column {.right}

## Third Column {.bottom}

Third/bottom column spans across the columns above.

# Vertical Slides

Place a new slide below the current slide using the `sub` tag.

```{markdown}
# Main Slide

This is the text of the main slide.

# Vertical Slide {.sub}

This is the text of the slide below.
```
 
# Main Slide

This is the text of the main slide.

# Vertical Slide {.sub}

This is the text of the slide below.

# Text Emphasis {#textEmphasis}

Format text by surrounding it in appropriate symbols:

##  {.split}

``` {.markdown}
**This is bold text**
__This is bold text__
*This is italic text*
_This is italic text_
~~Strikethrough~~
<u>underline</u>
~subscript~
^superscript^
```

## 

**This is bold text**\
**This is bold text**\
*This is italic text*\
*This is italic text*\
~~Strikethrough~~\
`<u>`{=html}underline`</u>`{=html}\
H~2~O is a liquid.\
2^3^ equals 8.

# Inverse Colors {#inverse .inverse background-color="black"}

## Color Scheme for Dark Images

-   Add `.inverse` tag to slide header (h1)
-   Add `background-color="black"` to slide header (h1)

## Even colored boxes look ok. {.fragment .alert}

# Highlight Blocks {#blocks}

##  {.split}

``` {.markdown}
## Alert Block {.alert}

-  Alert Text
```

##  

``` {.markdown}
## Question Block {.question}

-  Question text
```

## 

``` {.markdown}
## Answer Block {.answer}

-  Answer text
```

##  

``` {.markdown}
## Definition Block {.definition}

-  Definition text
```

##  

``` {.markdown}
## Observation Block {.observation}

-  Observation text
```

##  

``` {.markdown}
## Example Block {.example}

-  Example text
```

##  

``` {.markdown}
## Equation Block {.equation}

-  Equation text
```

##  

``` {.markdown}
## Note Block {.note}

-  Note text
```

# Highlight Blocks example {#example-blocks}

## Alert Block {.alert .split}

-   Alert Text

## Question Block {.question}

-   Question text

## Answer Block {.answer}

-   Answer text

## Definition Block {.definition}

-   Definition text

## Observation Block {.observation}

-   Observation text

## Example Block {.example}

-   Example text

## Equation Block {.equation}

-   Equation text

## Note Block {.note}

-   Note text

# Lists

## Ordered Lists {.split}

``` {.markdown}
1.  bread
2.  milk
3.  sugar
4.  flour
```

##  

1.  bread
2.  milk
3.  sugar
4.  flour

## Enumerated Lists

``` {.markdown}
-  Take out trash
-  Vaccuum
    - Bedrooms
-  Wash dishes
```

## 

-   Take out trash
-   Vaccuum
    -   Bedrooms
-   Wash dishes

# Sequential Lists {#seqlists}

Use the (@) symbol to automatically number items in a list.\
Numbered examples do not need to be in a single list.

##  {style="font-size:xx-large;"}

``` {.markdown}
(@)  Salman Rushdie, *The Ground beneath Her Feet* (New York: Henry Holt, 1999), 25.  

(@)  Bob Stewart, "Wag of the Tail: Reflecting on Pet Ownership," in *Enriching Our
  Lives with Animals*, ed. John Jaimeson, Tony Bannerman and Selena Wong
  (Toronto, ON: Petlove Press, 2007),100.  

Additional sources:  

(@)  Elliot Antokoletz, *Musical Symbolism in the Operas of Debussy and Bartok*
  (New York: Oxford University Press, 2008),
  doi:10.1093/acprof:oso/9780195365825.001.0001.
```

# Sequential Lists example {#example-seqlists}

(1) Salman Rushdie, *The Ground beneath Her Feet* (New York: Henry Holt,
    1999), 25.

(2) Bob Stewart, "Wag of the Tail: Reflecting on Pet Ownership," in
    *Enriching Our Lives with Animals*, ed. John Jaimeson, Tony
    Bannerman and Selena Wong (Toronto, ON: Petlove Press, 2007),100.

Additional sources:

(3) Elliot Antokoletz, *Musical Symbolism in the Operas of Debussy and
    Bartok* (New York: Oxford University Press, 2008),
    doi:10.1093/acprof:oso/9780195365825.001.0001.

# Links

Enter the text to be displayed followed by the URL or slide ID.

``` {.markdown}
[text-to-be-displayed](https://url-of-website)
[text-to-be-displayed](#slide-id)
```

*Note:* Slide IDs are entered on the slide header (h1) as follows:

``` {.markdown}
# Slide Title {#slide-id}
```

# Links example {#example-links}

``` {.markdown}
Visit [http://pandoc.org](http://pandoc.org) for additional information.

Read more about building [lists](#lists) in Decker.
```

##  {.example}

Visit <http://pandoc.org> for additional information.\
Read more about building [lists](#lists) in Decker.

# Images

Include images in presentations:

``` {.markdown}
![Image Caption](image-file-location){css-formatting}
```

# Images example {#example-images}

## 

``` {.markdown}
![JMU Würzburg HCI](img/title-logo-hci.png){width="30%"}
```

## 

![JMU Würzburg HCI](img/title-logo-hci.png){width="30%"}

# Videos {#video}

Include videos in presentations:

``` {.markdown}
![title](video-file-location){css-formatting}
```

# Videos example {#example-movies_1}

``` {.markdown}
Video with controls:
![](movies/jmu-hci-intro.mp4){.controls}

Video with autoplay:
![](movies/jmu-hci-intro.mp4){.autoplay}

Start video at timestamp:
![](movies/jmu-hci-intro.mp4){.autoplay start="10"}
```

##  {.split}

Video with controls: ![](movies/jmu-hci-intro.mp4){.controls width="80%"}

## 

Video with autoplay & loop: ![](movies/jmu-hci-intro.mp4){.autoplay .loop width="80%"}

# External Videos {#ext-vid}

Include YouTube and Vimeo videos or Twitch channels in presentations:

``` {.markdown}
![](service:video-id){css-formatting}
```

- Replace `service` with `youtube`, `vimeo` or `twitch` and add
video id or twitch channel name (replaces `video-id`).

- The video ID is usually found in the URL.

**YouTube example URL:**
https://www.youtube.com/watch?v=`<u>`{=html}qEcmwHRG2Mo`</u>`{=html}\
**YouTube video ID:** qEcmwHRG2Mo

# External Videos example {#example-movies_2}

``` {.markdown}
![](youtube:qEcmwHRG2Mo){width="65%" start="10"}
```

![](youtube:qEcmwHRG2Mo){width="65%" start="10"}

# Fullscreen Videos {#fullscreen}

Fullscreen videos are identified in the slide header:

``` {.markdown}
# ![](movies/jmu-hci-intro.mp4){.controls}
```

*Note:* Do not include a slide title.

# ![](movies/jmu-hci-intro.mp4) {#example-movies_3 data-menu-title="Fullscreen Videos Example" .controls}

# Audio

Include audio clips in presentations:

``` {.markdown}
![title](audio-file-location){css-formatting}
```

# Audio example {#example-audio .columns}

## {.top}

``` {.markdown .xxx-small}
Example 1: audio with controls
![Wildbach](audio/wildbach.mp3){.controls}
```
``` {.markdown .xxx-small}
Example 2: audio with controls, autoplay and muted
![Wildbach](audio/wildbach.mp3){.controls .autoplay .muted}
```

##  {.left .example}

Example 1: ![Wildbach](audio/wildbach.mp3){.controls}

##  {.right .example}

Example 2: ![Wildbach](audio/wildbach.mp3){.controls .autoplay .muted}

# Tables

Tables are created with pipes (\|) and hyphens (-). Align text with
colons (:) on the left, right, or on both sides of the hyphens in the
header row.

``` {.markdown}
| Right Align | Left Align | Center Align | Default |
|        ---: | :---       |    :---:     | ------- |
|        data | data       |     data     | data    |
|        data | data       |     data     | data    |
```

# Tables example {#example-tables}

##  

``` {.markdown}
Table: Assignment List

|  Week | Topic | Reading | Book |
|  ---: | :---  |  :---:  | ---- |
|   1   | Course Introduction | Chapt. 1 | Physics |
|   2   | Inertia, Equilibrium, Kinematics | Chapt. 2-3| Physics |
|   3   | Vectors, Momentum, Energy | Chapt. 4-7 | Physics |
```

## 

##  {.example}

|  Week| Topic                            |     Reading    | Book    |
|-----:|:---------------------------------|:--------------:|---------|
|     1| Course Introduction              |    Chapt. 1    | Physics |
|     2| Inertia, Equilibrium, Kinematics | Chapt. 2, 3, 4 | Physics |
|     3| Vectors, Momentum, Energy        |   Chapt. 5-8   | Physics |

: Assignment List

# Verbatim Code Blocks {#code}

To treat text as verbatim, either:

-   surround text with three tildes ( \~ ) or backticks ( \` )\
-   or indent each line by four spaces.

# Verbatim Code Block example {#example-code}

## Example Markdown:

``` {.markdown}
~~~java
if (a > 3) {
  moveShip(5 * gravity, DOWN);
}
~~~
```

## Code: 

``` {.java}
if (a > 3) {
  moveShip(5 * gravity, DOWN);
}
```

# Block Quotes {#blockQuote}

To quote a block of text, preceed each line with a (\>) character:

``` {.markdown}
> This is a block quote.
>
> > A block quote within a block quote.
```

> This is a block quote.
>
> > A block quote within a block quote.

# Mathematics {#math layout="columns"}

##  {.top}

-   Single \$ encloses inline math
-   Double \$\$ encloses a display math block

##  {.left}

``` {.latex}
To $\infty$ and beyond!
```

``` {.latex}
$$ e = mc ^ 2 $$
```

``` {.latex}
\lim_{x \to \infty} \exp(-x) = 0
```

##  {.right}

##  {.example}

To $\infty$ and beyond!

##  {.example}

$$ e = mc ^ 2 $$

##  {.example}

$$ \lim_{x \to \infty} \exp(-x) = 0 $$

# Java Syntax Highlighting {#java}

Apply Java syntax highlighting with the `.Java` tag.

## Example Markdown:

``` {.markdown}
~~~java
String s = "Java highlighting syntax";
System.out.println (s);
~~~
```

## Code: 

``` {.java}
String s = "Java highlighting syntax";
System.out.println (s);
```

# Javascript Syntax Highlighting {#javascript}

Apply Javascript syntax highlighting with the `.Javascript` tag.

## Example Markdown:

``` {.markdown}
~~~javascript
var s = "JavaScript syntax highlighting";
alert (s);
~~~
```

## Code:

``` {.javascript}
var s = "JavaScript syntax highlighting";
alert (s);
```

# Embed External Websites {#externalWebite}

-   Paste the following iframe on a blank slide\
-   Change "https://www.uni-wuerzburg.de/" to your website 

## 

``` {.html}
<iframe class = "stretch" src = "https://www.uni-wuerzburg.de/"></iframe>
```

#  {#example-externalWebsite data-menu-title="External Website Example"}

```{=html}
<iframe src="https://www.uni-wuerzburg.de/">
```
```{=html}
</iframe>
```
# Embed PDF documents {#embedPDF style="font-size:small;"}

``` {.markdown}
![](http://pandoc.org/MANUAL.pdf){width="100%" height="500px"}
```

![](http://pandoc.org/MANUAL.pdf){width="100%" height="500px"}

```{=html}
<!-- The given path (../../resource/support/...) won't work; therefore I excluded this part for now
# Embed JavaScript {#embedJavascript}

![](webgl_geometry_minecraft_ao.html){.iframe width="100%" height="500px"}

[](../../resource/support/three.js){.resource}

# ![](webgl_geometry_minecraft_ao.html)
-->
```

# PDF Documents in Header

``` {.markdown}
# ![](http://pandoc.org/MANUAL.pdf)
```

# ![](http://pandoc.org/MANUAL.pdf)


# Whiteboard

Dynamically make notes on presentations:

-   Show the whiteboard menu: type `w` 
-   Change pen color: type a number `1` - `7`
-   Change pen size: type a number `8` - `0`
-   Clear the whiteboard: press the `Del` key
-   Toggle the laser pointer: type `l`
-   Use the eraser: click `<i class="fas fa-eraser">`{=html}`</i>`{=html}
-   Download notes: click `<i class="fas fa-save">`{=html}`</i>`{=html}
-   Extend the whiteboard: click `<i class="fas fa-plus">`{=html}`</i>`{=html}
-   Display gridlines: click `<i class="fas fa-border-all">`{=html}`</i>`{=html}

# Speaker Notes {#speakerNotes}

Slides with headers with the `.notes` tag are not included in a
presentation. They only appear in the handout and in the speaker view
(press `s` on this slide to access the speaker view).

## 

``` {.markdown}
# Why Gamify? {.notes}

- Games are among the most powerful motivational tools.
- Make the non-game experience more rewarding.
- Motivation has limits. A large leaderboard divide may
  cause the player to abandon the game.
```

# Why Gamify? {.notes}

-   Games are among the most powerful motivational tools.

-   Make the non-game experience more rewarding

-   Motivation has limits. A large leaderboard divide may cause the
    player to abandon the game.

    -   [Blockquotes](#block-quotes)
    -   [Line Blocks](#line-blocks)
    -   [Tags](#tags)

# Citations

Add citations to your slide deck. Be sure to include a `csl` and a `bib`
file in your [YAML header](#yaml).

``` {.markdown}
## Space Tentacles

Have you heard about Space Tentacles [@zimmerer2018space].
According to @zimmerer2018space it is a nice idea.
```

## Space Tentacles

Have you heard about Space Tentacles [@zimmerer2018space]. According to
@zimmerer2018space it is a nice idea.

# References
