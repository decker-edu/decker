## Decker

This project is built with [Decker](https://github.com/decker-edu/decker).
Before creating or editing `*-deck.md`, `*-page.md` or `*-quest.yaml` files:

- Read the guide of the installed version: `decker agent-guide`, then the
  topic you need (e.g. `decker agent-guide decks`).
- Get project facts with `decker info --json`.
- After every change, run `decker --strict public/<path>.html` and fix all
  reported problems (exit 1) before reporting done.
- `decker serve`, `decker -S`, `decker -w` and `decker exam-builder` do not
  return. Never run them in the foreground; `decker agent-guide` explains when
  to run `decker serve -p <port>` in the background.
