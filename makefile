base-name := decker
executable := $(shell stack path | grep local-install-root | sed "s/local-install-root: //")/bin/decker
version := $(shell grep "version: " package.yaml | sed "s/version: *//")
branch := $(shell git rev-parse --abbrev-ref HEAD)
commit := $(shell git rev-parse --short HEAD)
local-bin-path := $(HOME)/.local/bin

decker-name := $(base-name)-$(version)-$(branch)-$(commit)

stack-build-opts :=  --fast --file-watch --ghc-options "-j4 +RTS -A128m -n2m -qg -RTS"

build: 
	stack build $(stack-build-options)

clean-build: clean
	git submodule update --init
	make -f symlinks.mk -C third-party all
	stack build $(stack-build-options)

less:
	stack build $(stack-build-options) 2>&1 | less 

install: clean-build
	mkdir -p $(local-bin-path)
	cp $(executable) "$(local-bin-path)/$(decker-name)"
	ln -sf "$(decker-name)" $(local-bin-path)/$(base-name)
	ln -sf "$(decker-name)" $(local-bin-path)/$(base-name)-$(version)

version:
	@echo "$(decker-name)"

build-profile:
	stack build --work-dir .stack-work-profile --profile

profile: build-profile
	stack exec -- decker clean
	stack exec --work-dir .stack-work-profile -- decker +RTS -p

preextracted: resources
	stack build --flag decker:preextractedresources

dist: install
	rm -rf dist
	mkdir -p dist
	ln -s $(executable) dist/$(decker-name)
	zip -qj dist/$(decker-name).zip dist/$(decker-name)
	rm dist/$(decker-name)

test:
	stack test

watch:
	stack test --file-watch

clean:
	stack clean
	rm -rf dist public
	rm -rf resource/support/vendor

.PHONY: build clean test install dist docs resources preextracted
