decker := $(shell stack path | grep local-install-root | sed "s/local-install-root: //")/bin/decker
version := $(shell grep "version: " package.yaml | sed "s/version: *//")

build:
	stack build -j 8 --fast

dist: build
	rm -rf dist
	mkdir -p dist
	ln -s $(decker) dist/decker-$(version) 
	zip -qj dist/decker-$(version).zip dist/decker-$(version)
	rm dist/decker-$(version)

test:
	stack test -j 8 --fast

watch:
	stack test -j 8 --fast --file-watch

clean:
	stack clean
	rm -rf dist

install: build
	stack exec -- decker clean
	stack install

.PHONY: build clean test install dist docs
