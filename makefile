decker := $(shell stack path | grep local-install-root | sed "s/local-install-root: //")/bin/decker
zip := $(shell mktemp -u)

build:
	stack build
	@(cd resource; zip -qr $(zip) example support template; cat $(zip) >> $(decker); rm $(zip))

clean:
	stack clean

install: build clean
	stack install

dist: clean build

info:
	@echo "decker: $(decker)"
	@echo "zip: $(zip)"

.PHONY: build clean install dist
