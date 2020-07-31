---
subtitle: To be discussed on the 31st of July 2020
title: The Future
---

# Pre-flight notes

## Wo are our customers?

We have a rather complex customer base and we want to keep everybody
happy:

1)  Core developers nerds that produce lots of content (Marc, Mario and
    me)
2)  Users that produce content for Marc (How many are there?)
3)  Users on the outside that just use decker (How many are there?)

Currently, I fear, 1), 2) and 3) seem mostly to be working with ad-hoc
builds cut directly from `master` or some other branch.

## Development model (as proposed by Henrik)

a)  A better integration process that reduces the number of problems
    caused by feature merges.

b)  Developer resource management. We need to prioritise issues and
    feature requests more strictly to prevent wasting energy on things
    that are not a top priority, and to enable a more timely reaction to
    important issues and critical bugs.

c)  Automated testing. Currently only very little code is unit tested on
    check-in. We need considerably more tests. They would have helped to
    prevent situations like \#268. (This would help with a))

d)  Manual testing. Many things cannot be tested automatically (with a
    reasonable amount of effort). To make manual testing more reliable,
    we need to implement more dedicated test cases and a manual testing
    protocol for reproducible results. (This also would help with a))

e)  Release management. More releases, more often. Whenever we have any
    interesting new features, we should make a new release. Minor
    release numbers a cheap and plentiful.

## Next steps (as proposed by Mario)

1)  Consolidate
    -   Merge stuff back into master. Since I was forced to switch back
        to my branch, you are missing several fixes and additions that
        we probably want to merge. We also should hunt down and fix bugs
        in this phase.
2)  Update
    -   There have been new releases for almost any third-party library
        we use, most notably Pandoc and Reveal. We should at least
        incorporate reveal's major version change, since this offers
        some cool things. It would also allow me to publish the math
        plugin and the whiteboard plugin to the Reveal community to get
        some Karma points.
3)  Finalize
    -   Finish some developments that are almost done, like
        `decker app`. There's probably more in this pre-final state.
4)  Invent
    -   What is needed in (our) online teaching, what can we implement
        until the winter term, and what are longer-term goals? I agree
        with Henrik that we need some method of prioritization.

## Plans for Winter 2020 (Henrik)

1.  [ ] `decker server` als Production Server
    -   [ ] Lasttests, besonders für Videos
    -   [ ] Feedback Service
    -   [ ] Quiz Service
2.  [ ] Eingebettete kurze Erklärvideos
    -   [ ] per Slide
    -   [ ] per Deck
    -   [ ] auch in Handouts
    -   [ ] Video Button oben in der Ecke
    -   [ ] Öffnet
        -   [ ] Full-screen Video Overlay (keine Slidenavigation)
        -   [ ] Kleine schwebende Videobriefmarke in der Ecke
            (Slidenavigation möglich)
3.  [ ] Feedback und Kommentare
    -   [ ] Volltext Feedback und Kommentare pro Slide und Deck
    -   [ ] Rating Page pro Deck (5-Star)
    -   [ ] Übertragung an den Server
    -   [ ] Statistische Auswertung der Ratings
    -   [ ] Veröffentlichungen der Ratings und der Kommentare
        (vielleicht moderiert) auf der Modulseite
4.  [ ] Selbsttest mit Auswertung
    -   [x] Import von Examiner (Klausurgenerierung) Fragen
    -   [x] Einbettung überall per Media Tag
        (`![](some-quest.yaml){.question}`)
    -   [ ] `Solve` Button
        -   [x] Anzeige Lösung, danach gesperrt
        -   [ ] Anzeige Punkte für Aufgabe, Punkte insgesamt
    -   [ ] Testauswertung am Ende des Decks
    -   [ ] Auch im Handout
    -   [ ] Export des gesamten Fragenkatalogs nach *Moodle XML Format*
        (`decker exam-catalog`)
5.  [ ] Path Tracer App (C++ -\> WASM)
    -   [ ] Einbettung per iFrame
    -   [ ] Parallelisierung per WebWorker
    -   [ ] Interaktion per editerbarem Textfeld
        -   [ ] Parsen der Parameter aus YAML format

# Attendance per (Skype, Zoom, Jitsy)
