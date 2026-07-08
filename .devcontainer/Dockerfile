# Dev container image for developing Decker.
#
# Provides the full Haskell toolchain (GHC 9.8.4 to match stack resolver
# lts-23.28), plus the native tools Decker shells out to at runtime
# (sassc, graphviz, gnuplot, rsync). Node + Claude Code are layered on top
# via devcontainer features / the post-create script.

FROM mcr.microsoft.com/devcontainers/base:ubuntu-24.04

# --- System dependencies -----------------------------------------------------
# Build-time deps for GHC/HLS (build-essential, libgmp, libffi, libncurses,
# zlib) and Decker's runtime tools (sassc, graphviz, gnuplot, rsync, zip).
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
    && sed -i 's/# de_DE.UTF-8 UTF-8/de_DE.UTF-8 UTF-8/' /etc/locale.gen \
    && sed -i 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen \
    && locale-gen \
    && rm -rf /var/lib/apt/lists/*

# Decker's Markdown parser expects a UTF-8 locale (umlauts).
ENV LANG=en_US.UTF-8 \
    LC_ALL=en_US.UTF-8

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
