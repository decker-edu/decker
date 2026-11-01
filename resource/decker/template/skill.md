---
name: decker
description: >-
  Authoring and validating Decker lecture content — slide decks (*-deck.md),
  pages (*-page.md) and exam questions (*-quest.yaml) that the `decker` build
  tool compiles to HTML. Use whenever creating or editing these files, or when
  the user mentions decker, slides, lecture decks or exam questions.
---

# Decker content authoring

Decker's syntax depends on its version, the resource pack and `decker.yaml`.
Do not rely on memory; ask the installed binary.

1. Before authoring, read the guide: `decker agent-guide` (index and build
   loop), then the topic you need, e.g. `decker agent-guide decks`.
2. For project facts (targets, resource pack, configuration, available
   fragment templates) run `decker info --json`.
3. After every change, build the target in strict mode and read the exit
   code: `decker --strict public/<path>.html`. Exit 1 means a hard error or a
   reported content problem (`[WARNING] ... [code]`). Fix it before reporting
   done.
4. `decker serve`, `decker -S`/`--server`, `decker -w`/`--watch` and
   `decker exam-builder` do not return. Never run them in the foreground; the
   guide explains when to run `decker serve -p <port>` in the background.
