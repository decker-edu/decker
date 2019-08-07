base-name := decker
executable := $(shell stack path | grep local-install-root | sed "s/local-install-root: //")/bin/decker
version := $(shell grep "version: " package.yaml | sed "s/version: *//")
branch := $(shell git rev-parse --abbrev-ref HEAD)
commit := $(shell git rev-parse --short HEAD)
local-bin-path := $(HOME)/.local/bin

decker-name := $(base-name)-$(version)-$(branch)-$(commit)
resource-dir := $(HOME)/.local/share/$(decker-name)

JS_DEP_COPY += decker.js
JS_DEP_COPY += page.js
JS_DEP_COPY += quiz.js
JS_DEP_COPY += handout.js
JS_DEP_COPY += csv2chart.js
JS_DEP_COPY += decker.css
JS_DEP_COPY += handout.css
JS_DEP_COPY += page.css
JS_DEP_COPY += chalkboard.js
FONTS = $(shell find src-support/fonts -type f)
JS_DEP_COPY += $(FONTS:src-support/%=%)
JS_DEP_COPY_FULL_PATH = $(addprefix resource/support/, $(JS_DEP_COPY))

less:
	stack build -j 8 --fast 2>&1 | less

build:
	stack build -j 8 --fast

resources: $(JS_DEP_COPY_FULL_PATH)
	$(MAKE) -C third-party support

dist: resources build
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
	rm -rf resource/support/*

build-profile:
	stack build -j 8 --fast --work-dir .stack-work-profile --profile

profile: build-profile
	stack exec -- decker clean
	stack exec --work-dir .stack-work-profile -- decker +RTS -p

preextracted:
	stack build -j 8 --fast --flag decker:preextractedresources

install: resources build
	stack exec -- decker clean
	mkdir -p $(local-bin-path)
	cp $(executable) "$(local-bin-path)/$(decker-name)"
	ln -sf "$(decker-name)" $(local-bin-path)/$(base-name)
	ln -sf "$(decker-name)" $(local-bin-path)/$(base-name)-$(version)

watch-resources:
	find resource src-support -name "*.scss" -or -name "*.html" -or -name "*.js" | entr -pd make install-resources

install-resources: resources
	rsync -r resource/ $(resource-dir)

version:
	@echo "$(decker-name)"

.PHONY: build clean test install dist docs resources preextracted

resource/support/decker.js: src-support/decker.js
	mkdir -p $(@D) && cp $< $@

resource/support/chalkboard.js: src-support/chalkboard.js
	mkdir -p $(@D) && cp $< $@

resource/support/page.js: src-support/page.js
	mkdir -p $(@D) && cp $< $@

resource/support/handout.js: src-support/handout.js
	mkdir -p $(@D) && cp $< $@

resource/support/quiz.js: src-support/quiz.js
	mkdir -p $(@D) && cp $< $@

resource/support/Chart.js: src-support/Chart.min.js
	mkdir -p $(@D) && cp $< $@

resource/support/csv2chart.js: src-support/csv2chart.js
	mkdir -p $(@D) && cp $< $@

resource/support/fonts/%: src-support/fonts/%
	mkdir -p $(@D) && cp $< $@

resource/support/decker.css: src-support/decker.scss
	sassc $< $@

resource/support/handout.css: src-support/handout.scss
	sassc $< $@

resource/support/page.css: src-support/page.scss
	sassc $< $@