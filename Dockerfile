FROM ubuntu:focal as base
LABEL maintainer="jan-philipp.stauffert@uni-wuerzburg.de"

RUN 

RUN export DEBIAN_FRONTEND=noninteractive && apt-get update &&	apt-get install -y \
    wget unzip zip libbz2-dev curl gnupg \
    sassc

RUN curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add - && \
    echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list && \
    export DEBIAN_FRONTEND=noninteractive && apt-get update && \
    export DEBIAN_FRONTEND=noninteractive && apt-get install -y yarn npm && npm install npm@6.13.4 -g


#set the encoding on UTF-8, so the parser works correctly, german language is also added for umlaute
#source of fix: https://blog.mkowalski.net/2016/05/16/solving-locale-issues-with-docker-containers/
RUN export DEBIAN_FRONTEND=noninteractive && apt-get install --reinstall -y locales && \
    sed -i 's/# de_DE.UTF-8 UTF-8/de_DE.UTF-8 UTF-8/' /etc/locale.gen && \
    locale-gen de_DE.UTF-8
ENV LANG de_DE.UTF-8
ENV LANGUAGE de_DE
ENV LC_ALL de_DE.UTF-8
RUN dpkg-reconfigure --frontend noninteractive locales

RUN wget -qO- https://get.haskellstack.org/ | sh

FROM base as deps

WORKDIR /deps
COPY ./stack.yaml stack.yaml
COPY ./package.yaml package.yaml
RUN stack build --only-snapshot

FROM deps as build

WORKDIR /decker
COPY . /decker
ARG MAKE_FLAGS
RUN make ${MAKE_FLAGS} install

RUN ldd /root/.local/bin/decker | grep "=> /" | awk '{print $3}' | xargs -I '{}' cp -v '{}' /root/.local/bin

FROM ubuntu:jammy as decker

# Base runtime tools decker shells out to: graphviz (dot), gnuplot, rsync,
# unzip/zip, plus what's needed to fetch/install the tools below
# (ca-certificates/curl/gnupg for the NodeSource and d2 install scripts).
RUN export DEBIAN_FRONTEND=noninteractive && apt-get update && apt-get install -y \
    graphviz \
    gnuplot \
    rsync \
    unzip \
    zip \
    ca-certificates \
    curl \
    gnupg \
    default-jre-headless \
    plantuml \
    texlive-xetex \
    texlive-latex-recommended \
    texlive-latex-extra \
    texlive-fonts-recommended \
    lmodern \
    pdf2svg \
    ffmpeg \
    && rm -rf /var/lib/apt/lists/*

# d2 (diagram renderer). Not packaged for Ubuntu; install from upstream's
# release assets, which cover both amd64 and arm64.
RUN curl -fsSL https://d2lang.com/install.sh | sh -s -- --force

# Node.js (LTS), needed only to install mermaid-cli and Playwright below;
# focal's own "nodejs" apt package is far too old for either.
RUN curl -fsSL https://deb.nodesource.com/setup_20.x | bash - \
    && export DEBIAN_FRONTEND=noninteractive && apt-get install -y nodejs \
    && rm -rf /var/lib/apt/lists/*

# mermaid-cli (`mmdc`, the "mermaid" external tool) and Playwright, which
# provides the headless Chromium backing both `mmdc` and decker's `chrome`
# external tool (used by `decker pdf`/`decker pdf-decks`). Playwright's own
# Chromium build (unlike Google Chrome's or Puppeteer's default download)
# is available for arm64 as well as amd64. PUPPETEER_SKIP_DOWNLOAD and
# PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD stop each package's own postinstall from
# fetching a redundant (and, on arm64, unavailable) browser of its own.
ENV PUPPETEER_SKIP_DOWNLOAD=true \
    PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD=1 \
    PUPPETEER_EXECUTABLE_PATH=/usr/local/bin/chrome
RUN npm install -g @mermaid-js/mermaid-cli playwright \
    && npx --yes playwright install --with-deps chromium \
    && ln -sf "$(NODE_PATH="$(npm root -g)" node -e 'console.log(require("playwright").chromium.executablePath())')" /usr/local/bin/chrome \
    && npm cache clean --force \
    && rm -rf /var/lib/apt/lists/*

ENV PATH="/root/.local/bin:${PATH}"
COPY --from=build /root/.local /root/.local

#set the encoding on UTF-8, so the parser works correctly, german language is also added for umlaute
#source of fix: https://blog.mkowalski.net/2016/05/16/solving-locale-issues-with-docker-containers/
RUN export DEBIAN_FRONTEND=noninteractive && apt-get install --reinstall -y locales && \
    sed -i 's/# de_DE.UTF-8 UTF-8/de_DE.UTF-8 UTF-8/' /etc/locale.gen && \
    locale-gen de_DE.UTF-8
ENV LANG de_DE.UTF-8
ENV LANGUAGE de_DE
ENV LC_ALL de_DE.UTF-8
RUN dpkg-reconfigure --frontend noninteractive locales

WORKDIR /decker

ENTRYPOINT ["decker"]
