FROM fpco/stack-build:lts-12.5

RUN mkdir /setup
WORKDIR /setup
ADD . /setup/
RUN stack build -j8 --only-dependencies --fast

ENTRYPOINT bash
