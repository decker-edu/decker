FROM ubuntu:focal as base
LABEL maintainer="jan-philipp.stauffert@uni-wuerzburg.de"

RUN apt-get update &&	apt-get install -y \
    wget unzip zip libbz2-dev curl gnupg \
    sassc nodejs

RUN curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add - && \
    echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list && \
    apt-get update && \
    apt-get install -y yarn npm && npm install npm@latest -g


#set the encoding on UTF-8, so the parser works correctly, german language is also added for umlaute
#source of fix: https://blog.mkowalski.net/2016/05/16/solving-locale-issues-with-docker-containers/
RUN apt-get install --reinstall -y locales && \
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

FROM ubuntu:focal as decker

RUN apt-get update && apt-get install -y \
    graphviz \
    gnuplot \
    rsync \
    unzip \
    zip

ENV PATH="/root/.local/bin:${PATH}"
COPY --from=build /root/.local /root/.local

#set the encoding on UTF-8, so the parser works correctly, german language is also added for umlaute
#source of fix: https://blog.mkowalski.net/2016/05/16/solving-locale-issues-with-docker-containers/
RUN apt-get install --reinstall -y locales && \
    sed -i 's/# de_DE.UTF-8 UTF-8/de_DE.UTF-8 UTF-8/' /etc/locale.gen && \
    locale-gen de_DE.UTF-8
ENV LANG de_DE.UTF-8
ENV LANGUAGE de_DE
ENV LC_ALL de_DE.UTF-8
RUN dpkg-reconfigure --frontend noninteractive locales

WORKDIR /decker

ENTRYPOINT ["decker"]
