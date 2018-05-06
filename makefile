decker := $(shell stack path | grep local-install-root | sed "s/local-install-root: //")/bin/decker
zip := $(shell mktemp -u)

build:
	stack build -j 8 --fast
	@(cd resource; zip -qr $(zip) example support template; cat $(zip) >> $(decker); rm $(zip))

test: build
	stack test -j 8 --fast

watch:
	stack test -j 8 --fast --file-watch

docs: build
	stack hoogle -- generate --local 

clean:
	stack clean

install: build
	stack exec -- decker clean
	stack install

dist: clean build

info:
	@echo "decker: $(decker)"
	@echo "zip: $(zip)"

.PHONY: build clean install dist docs
