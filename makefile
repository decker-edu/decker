base-name := decker
executable := $(shell stack path | grep local-install-root | sed "s/local-install-root: //")/bin/decker
version := $(shell grep "version: " package.yaml | sed "s/version: *//")
branch := $(shell git rev-parse --abbrev-ref HEAD)
commit := $(shell git rev-parse --short HEAD)
local-bin-path := $(HOME)/.local/bin

decker-name := $(base-name)-$(version)-$(branch)-$(commit)


build: css
	rm decker.cabal
	stack build

clean-build: clean css
	git submodule update --init
	make -f symlinks.mk -C third-party all
	rm decker.cabal
	stack build

less:
	rm decker.cabal
	stack build 2>&1 | less 

resource-zip:
	rm -f resource/decker-resources.zip
	(cd resource; zip -qr decker-resources.zip example support template tutorial)

install: clean-build
	mkdir -p $(local-bin-path)
	cp $(executable) "$(local-bin-path)/$(decker-name)"
	ln -sf "$(decker-name)" $(local-bin-path)/$(base-name)
	ln -sf "$(decker-name)" $(local-bin-path)/$(base-name)-$(version)

version:
	@echo "$(decker-name)"

build-profile:
	rm decker.cabal
	stack build --work-dir .stack-work-profile --profile

profile: build-profile
	stack exec -- decker clean
	stack exec --work-dir .stack-work-profile -- decker +RTS -p

dist: install
	rm -rf dist
	mkdir -p dist
	ln -s $(executable) dist/$(decker-name)
	zip -qj dist/$(decker-name).zip dist/$(decker-name)
	rm dist/$(decker-name)

test:
	stack test -j1

watch:
	stack test -j1 --file-watch

server:
	stack run -- decker server

css:
	cd resource/support/css && make css

clean:
	stack clean
	rm -rf dist public
	rm -rf resource/support/vendor

.PHONY: build clean test install dist docs resource-zip css
