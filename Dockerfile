# Builds the decker executable
FROM ubuntu:artful

RUN apt-get update &&	apt-get install -y \
  wget \
  unzip \
  zip \
  libbz2-dev 

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

WORKDIR /decker
COPY . /decker
RUN make install

RUN ldd /root/.local/bin/decker | grep "=> /" | awk '{print $3}' | xargs -I '{}' cp -v '{}' /root/.local/bin

# Image that will execute decker
FROM ubuntu:artful

RUN apt-get update && apt-get install -y \
    graphviz \
    gnuplot \
    rsync \
    sassc \
    unzip \
    zip

ENV PATH="/root/.local/bin:${PATH}"
COPY --from=0 /root/.local /root/.local
#COPY --from=0 /root/.stack/snapshots/x86_64-linux-nopie/lts-11.6/8.2.2/share/x86_64-linux-ghc-8.2.2/pandoc-citeproc-0.14.3/locales/locales-en-US.xml /root/.stack/snapshots/x86_64-linux-nopie/lts-11.6/8.2.2/share/x86_64-linux-ghc-8.2.2/pandoc-citeproc-0.14.3/locales/locales-en-US.xml

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
