# Dev container image for developing Decker.
#
# Provides the full Haskell toolchain (GHC 9.8.4 to match stack resolver
# lts-23.28), plus the native tools Decker shells out to at runtime
# (sassc, graphviz, gnuplot, plantuml, pdflatex/xelatex, pdf2svg, ffmpeg,
# d2, rsync, headless chrome, mermaid). Node + Claude Code are layered on
# top via devcontainer features / the post-create script; the
# Node-dependent tools (mermaid-cli, headless chrome) are installed there
# too, see post-create.sh.

FROM mcr.microsoft.com/devcontainers/base:ubuntu-24.04

# --- System dependencies -----------------------------------------------------
# Build-time deps for GHC/HLS (build-essential, libgmp, libffi, libncurses,
# zlib) and Decker's runtime tools:
#   sassc, graphviz (dot), gnuplot, rsync, zip, pdf2svg, ffmpeg
#   default-jre-headless + plantuml    -> "plantuml" renderer
#   texlive-xetex                      -> xelatex, used by the PDF page/handout writer
#   texlive-latex-recommended/-extra,
#   texlive-fonts-recommended, lmodern -> pdflatex renderer + fonts/packages
#     pulled in by pandoc's default LaTeX template
RUN export DEBIAN_FRONTEND=noninteractive \
    && apt-get update \
    && apt-get install -y --no-install-recommends \
        build-essential \
        curl \
        git \
        pkg-config \
        libffi-dev \
        libffi8 \
        libgmp-dev \
        libgmp10 \
        libncurses-dev \
        libtinfo6 \
        zlib1g-dev \
        libbz2-dev \
        sassc \
        graphviz \
        gnuplot \
        rsync \
        unzip \
        zip \
        locales \
        default-jre-headless \
        plantuml \
        texlive-xetex \
        texlive-latex-recommended \
        texlive-latex-extra \
        texlive-fonts-recommended \
        lmodern \
        pdf2svg \
        ffmpeg \
    && sed -i 's/# de_DE.UTF-8 UTF-8/de_DE.UTF-8 UTF-8/' /etc/locale.gen \
    && sed -i 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen \
    && locale-gen \
    && rm -rf /var/lib/apt/lists/*

# d2 (diagram renderer). Installed from upstream's release assets rather
# than apt (not packaged for Ubuntu); the install script picks the right
# arch, including arm64.
RUN curl -fsSL https://d2lang.com/install.sh | sh -s -- --force

# Decker's Markdown parser expects a UTF-8 locale (umlauts).
ENV LANG=en_US.UTF-8 \
    LC_ALL=en_US.UTF-8

# Ubuntu only ships "chromium" as a snap-only transitional package (no real
# deb, on any arch), which doesn't work in a plain container. Headless
# Chrome is instead provisioned in post-create.sh via Playwright, whose
# Chromium build (unlike Google Chrome / Puppeteer's default download) is
# available for linux-arm64 as well as amd64. These vars make that
# Playwright-installed browser double as both `mmdc`'s (mermaid-cli) and
# decker's `chrome` external tool, and stop mermaid-cli from trying (and,
# on arm64, failing) to download its own copy.
ENV PUPPETEER_SKIP_DOWNLOAD=true \
    PUPPETEER_EXECUTABLE_PATH=/usr/local/bin/chrome

# --- Haskell toolchain (as the non-root user) --------------------------------
USER vscode

ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_GHC_VERSION=9.8.4 \
    BOOTSTRAP_HASKELL_INSTALL_STACK=1 \
    BOOTSTRAP_HASKELL_INSTALL_HLS=1 \
    BOOTSTRAP_HASKELL_ADJUST_BASHRC=1

# ghcup installs GHC 9.8.4, cabal, stack and HLS into ~/.ghcup and wires up a
# stack hook so `stack` reuses the ghcup-managed GHC instead of downloading its
# own. That keeps the LSP (HLS) and the build using the exact same compiler.
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

ENV PATH=/home/vscode/.ghcup/bin:/home/vscode/.local/bin:$PATH

# Make stack reuse the system/ghcup GHC rather than provisioning its own.
RUN stack config set system-ghc true --global \
    && stack config set install-ghc false --global
