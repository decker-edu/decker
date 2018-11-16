base-name := decker
executable := $(shell stack path | grep local-install-root | sed "s/local-install-root: //")/bin/decker
version := $(shell grep "version: " package.yaml | sed "s/version: *//")
branch := $(shell git branch | grep \* | cut -d ' ' -f2)
local-bin-path := $(HOME)/.local/bin

ifeq ($(branch),master)
	decker-name := $(base-name)-$(version)
else
	decker-name := $(base-name)-$(version)-$(branch)
endif

resource-dir := $(HOME)/.local/share/decker-$(version)
local-bin-path := ~/.local/bin

less:
	stack build -j 8 --fast 2>&1 | less

build:
	stack build -j 8 --fast

yarn:
	yarn --silent install && yarn --silent run webpack --mode production --silent
	cp -r node_modules/reveal.js-menu resource/support/

dist: yarn build
	rm -rf dist
	mkdir -p dist
	ln -s $(executable) dist/$(decker-name) 
	zip -qj dist/$(decker-name).zip dist/$(decker-name)
	rm dist/$(decker-name)

test:
	stack test -j 8 --fast

watch:
	stack test -j 8 --fast --file-watch

clean:
	stack clean
	rm -rf dist

build-profile:
	stack build -j 8 --fast --work-dir .stack-work-profile --profile

profile: build-profile
	stack exec -- decker clean
	stack exec --work-dir .stack-work-profile -- decker +RTS -p

install: yarn build
	stack exec -- decker clean
	cp $(executable) "$(local-bin-path)/$(decker-name)"
	ln -sf "$(decker-name)" $(local-bin-path)/$(base-name)

watch-resources:
	find resource src-support -name "*.scss" -or -name "*.html" -or -name "*.js" | entr -pd make install-resources

install-resources: yarn
	rsync -r resource $(resource-dir)

version:
	@echo "$(decker-name)"

.PHONY: build clean test install version install-resources watch watch-resources dist docs yarn build-profile profile
