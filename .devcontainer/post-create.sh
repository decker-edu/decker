#!/usr/bin/env bash
# Runs once after the container is created (postCreateCommand).
set -euo pipefail

# Named volumes are created root-owned; hand them to the dev user.
sudo chown -R vscode:vscode \
  /home/vscode/.stack \
  /home/vscode/.claude \
  "$PWD/.stack-work" 2>/dev/null || true

# The MathJax submodule uses an SSH URL. Rewrite GitHub SSH -> HTTPS so
# `git submodule update --init` works without forwarding an SSH key.
git config --global url."https://github.com/".insteadOf "git@github.com:"

# Install Claude Code CLI (usable from Zed's integrated terminal, which runs
# inside this container).
npm install -g @anthropic-ai/claude-code

# Fetch git submodules (reveal.js, Font-Awesome, MathJax, codapi, codejar) and
# node deps needed by the support resources.
git submodule update --init --recursive
npm install

echo
echo "Dev container ready."
echo "  Build:  stack build -j8"
echo "  Test:   stack test -j1"
echo "  Serve:  stack run -- decker --server   (http://localhost:8888)"
echo "  Claude: run 'claude' in the terminal (auth persists in ~/.claude volume)"
