decker := $(shell stack path | grep local-install-root | sed "s/local-install-root: //")/bin/decker
version := $(shell grep "version: " package.yaml | sed "s/version: *//")
zip := $(shell mktemp -u)

build:
	stack build -j 8 --fast
	@(cd resource; zip -qr $(zip) example support template; cat $(zip) >> $(decker); rm $(zip))
	rm -rf dist
	mkdir -p dist
	ln -s $(decker) dist/decker-$(version) 
	zip -qj dist/decker.zip dist/decker-$(version)
	rm dist/decker-$(version)

test: build
	stack test -j 8 --fast

watch:
	stack test -j 8 --fast --file-watch

clean:
	stack clean
	rm -rf dist

install: build
	stack exec -- decker clean
	stack install

dist: clean build

info:
	@echo "decker: $(decker)"
	@echo "zip: $(zip)"

.PHONY: build clean install dist docs
