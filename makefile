stack-cmd ?= stack
stack-options ?=
stack-parallel ?= 8
base-name := decker
executable := $(shell $(stack-cmd) $(stack-options) path | grep local-install-root | sed "s/local-install-root: //")/bin/decker
version := $(shell grep "version: " package.yaml | sed "s/version: *//")
branch := $(shell git rev-parse --abbrev-ref HEAD)
commit := $(shell git rev-parse --short HEAD)
local-bin-path := $(HOME)/.local/bin

decker-name := $(base-name)-$(version)-$(branch)-$(commit)

.PHONY: build clean test install list dist docs resource-zip css

build: 
	$(stack-cmd) $(stack-options) build -j $(stack-parallel)

clean-build: clean 
	$(stack-cmd) $(stack-options) clean
	$(stack-cmd) $(stack-options) build -j $(stack-parallel)

upgrade-third-party:
	git submodule update --init
	make -C third-party -f makefile upgrade
 
less:
	$(stack-cmd) $(stack-options) build 2>&1 | less

resource-zip:
	rm -f resource/decker-resources.zip
	(cd resource; zip -qr decker-resources.zip example support template)

install: clean-build
	mkdir -p "$(local-bin-path)"
	rm -f "$(local-bin-path)/$(base-name)" 
	cp "$(executable)" "$(local-bin-path)/$(base-name)"

unclean-install: build
	mkdir -p "$(local-bin-path)"
	cp "$(executable)" "$(local-bin-path)/$(base-name)"

install-link: build
	mkdir -p "$(local-bin-path)"
	rm "$(local-bin-path)/$(base-name)-dev"
	ln -s "$(executable)" "$(local-bin-path)/$(base-name)-dev"

version:
	@echo "$(decker-name)"

build-profile:
	$(stack-cmd) $(stack-options) build --work-dir .$(stack-cmd) $(stack-options)-work-profile --profile

profile: build-profile
	$(stack-cmd) $(stack-options) exec -- decker clean
	$(stack-cmd) $(stack-options) exec --work-dir .$(stack-cmd) $(stack-options)-work-profile -- decker +RTS -p

dist: install
	rm -rf dist
	mkdir -p dist
	ln -s "$(executable)" "dist/$(decker-name)"
	zip -qj "dist/$(decker-name).zip" "dist/$(decker-name)"
	rm "dist/$(decker-name)"

test:
	$(stack-cmd) $(stack-options) test -j1

documentation:
	$(stack-cmd) $(stack-options) haddock

watch:
	$(stack-cmd) $(stack-options) test -j1 --file-watch

server:
	$(stack-cmd) $(stack-options) run -- decker --server --port 8888 --bind localhost html

clean:
	$(stack-cmd) $(stack-options) clean
	rm -rf dist public

append-webm:
	curl -T test/decks/movie.webm http://localhost:8888/append/test/decks/media-recording.webm
	# ls -rtl test/decks/media-recording*

replace-webm:
	curl -T test/decks/movie.webm http://localhost:8888/replace/test/decks/media-recording.webm
	curl -T test/decks/movie-1.webm http://localhost:8888/replace/test/decks/media-recording.webm
	# ls -rtl test/decks/media-recording*

clean-recordings:
	rm -f test/decks/*-recording*
	rm -f test/decks/*-times.json
	rm -f test/decks/*-annot.json
list:
	@LC_ALL=C $(MAKE) -pRrq -f $(lastword $(MAKEFILE_LIST)) : 2>/dev/null | awk -v RS= -F: '/(^|\n)# Files(\n|$$)/,/(^|\n)# Finished Make data base/ {if ($$1 !~ "^[#.]") {print $$1}}' | sort | egrep -v -e '^[^[:alnum:]]' -e '^$@$$'
