base-name := decker
executable := $(shell stack path | grep local-install-root | sed "s/local-install-root: //")/bin/decker
version := $(shell grep "version: " package.yaml | sed "s/version: *//")
branch := $(shell git rev-parse --abbrev-ref HEAD)
commit := $(shell git rev-parse --short HEAD)
local-bin-path := $(HOME)/.local/bin

decker-name := $(base-name)-$(version)-$(branch)-$(commit)
resource-dir := $(HOME)/.local/share/$(decker-name)

JS_DEP_COPY = notes/notes.html
JS_DEP_COPY += notes/notes.js
JS_DEP_COPY += reveal.js
JS_DEP_COPY += reveal.js-menu/menu.js
JS_DEP_COPY += reveal.js-menu/menu.css
JS_DEP_COPY += reveal.js-menu/font-awesome/css/all.css
JS_DEP_COPY += \
	reveal.js-menu/font-awesome/webfonts/fa-solid-900.woff2 \
	reveal.js-menu/font-awesome/webfonts/fa-regular-400.woff2 \
	reveal.js-menu/font-awesome/webfonts/fa-solid-900.woff \
	reveal.js-menu/font-awesome/webfonts/fa-regular-400.woff \
	reveal.js-menu/font-awesome/webfonts/fa-solid-900.ttf \
	reveal.js-menu/font-awesome/webfonts/fa-regular-400.ttf
JS_DEP_COPY += print/paper.css
JS_DEP_COPY += print/pdf.css
JS_DEP_COPY += jquery.js
JS_DEP_COPY += plugin/math.js
JS_DEP_COPY += plugin/zoom.js
JS_DEP_COPY += classList.js
JS_DEP_COPY += d3.js
JS_DEP_COPY += three.js
JS_DEP_COPY += head.js
JS_DEP_COPY += decker.js
JS_DEP_COPY += page.js
JS_DEP_COPY += quiz.js
JS_DEP_COPY += handout.js
JS_DEP_COPY += csv2chart.js
JS_DEP_COPY += Chart.js
JS_DEP_COPY += decker.css
JS_DEP_COPY += handout.css
JS_DEP_COPY += page.css
JS_DEP_COPY += reveal.css
JS_DEP_COPY += bootstrap.css
MATHJAX = node_modules/mathjax/MathJax.js
MATHJAX += node_modules/mathjax/config/TeX-AMS_SVG.js
MATHJAX += $(shell find node_modules/mathjax/jax/input/TeX -name "*.js")
MATHJAX += $(shell find  node_modules/mathjax/jax/output/SVG -name "*.js")
MATHJAX += $(wildcard node_modules/mathjax/extensions/*.js)
MATHJAX += $(shell find node_modules/mathjax/extensions/TeX -name "*.js")
MATHJAX += $(shell find node_modules/mathjax/jax/element -name "*.js")
JS_DEP_COPY += $(MATHJAX:node_modules/%=%)
FONTS = $(shell find src-support/fonts -type f)
JS_DEP_COPY += $(FONTS:src-support/%=%)
JS_DEP_COPY_FULL_PATH = $(addprefix resource/support/, $(JS_DEP_COPY))

less:
	stack build -j 8 --fast 2>&1 | less

build:
	stack build -j 8 --fast

resources: $(JS_DEP_COPY_FULL_PATH)

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

##### Copy JS dependencies that can't be packed with webpack

resource/support/decker.js: src-support/decker.js
	mkdir -p $(@D) && cp $< $@

resource/support/page.js: src-support/page.js
	mkdir -p $(@D) && cp $< $@

resource/support/handout.js: src-support/handout.js
	mkdir -p $(@D) && cp $< $@

resource/support/quiz.js: src-support/quiz.js
	mkdir -p $(@D) && cp $< $@

resource/support/bootstrap.css: node_modules/bootstrap/dist/css/bootstrap.min.css
	mkdir -p $(@D) && cp $< $@

resource/support/Chart.js: src-support/Chart.min.js
	mkdir -p $(@D) && cp $< $@

resource/support/csv2chart.js: src-support/csv2chart.js
	mkdir -p $(@D) && cp $< $@

resource/support/d3.js: node_modules/d3/dist/d3.min.js
	mkdir -p $(@D) && cp $< $@

resource/support/three.js: node_modules/three/build/three.min.js
	mkdir -p $(@D) && cp $< $@

resource/support/print/%: node_modules/reveal.js/css/print/%
	mkdir -p $(@D) && cp $< $@

resource/support/reveal.js: node_modules/reveal.js/js/reveal.js
	mkdir -p $(@D) && cp $< $@

resource/support/head.js: node_modules/reveal.js/lib/js/head.min.js
	mkdir -p $(@D) && cp $< $@

resource/support/classList.js: node_modules/reveal.js/lib/js/classList.js
	mkdir -p $(@D) && cp $< $@

resource/support/plugin/math.js: node_modules/reveal.js/plugin/math/math.js
	mkdir -p $(@D) && cp $< $@

resource/support/plugin/zoom.js: node_modules/reveal.js/plugin/zoom-js/zoom.js
	mkdir -p $(@D) && cp $< $@

resource/support/jquery.js: node_modules/jquery/dist/jquery.min.js
	mkdir -p $(@D) && cp $< $@

resource/support/notes/%: node_modules/reveal.js/plugin/notes/%
	mkdir -p $(@D) && cp $< $@

resource/support/reveal.js-menu/%: node_modules/reveal.js-menu/%
	mkdir -p $(@D) &&	cp $< $@

resource/support/mathjax/%: node_modules/mathjax/%
	mkdir -p $(@D) && cp $< $@

resource/support/decker.css: src-support/decker.css
	mkdir -p $(@D) && cp $< $@

resource/support/handout.css: src-support/handout.css
	mkdir -p $(@D) && cp $< $@

resource/support/page.css: src-support/page.css
	mkdir -p $(@D) && cp $< $@

resource/support/reveal.css: node_modules/reveal.js/css/reveal.css
	mkdir -p $(@D) && cp $< $@

node_modules/%:
	yarn install

SECONDARY = node_modules/reveal.js/js/reveal.js
SECONDARY += node_modules/reveal.js/css/reveal.scss
SECONDARY += node_modules/bootstrap/dist/css/bootstrap.min.css
SECONDARY += node_modules/reveal.js/lib/js/head.min.js
SECONDARY += node_modules/reveal.js/lib/js/classList.js
SECONDARY += node_modules/reveal.js/plugin/math/math.js
SECONDARY += node_modules/reveal.js/plugin/zoom-js/zoom.js
SECONDARY += node_modules/reveal.js/css/print/paper.css
SECONDARY += node_modules/reveal.js/css/print/pdf.css
SECONDARY += node_modules/reveal.js/plugin/notes/notes.html
SECONDARY += node_modules/reveal.js/plugin/notes/notes.js
SECONDARY += node_modules/d3/dist/d3.min.js
SECONDARY += node_modules/three/build/three.min.js
SECONDARY += node_modules/reveal.js-menu/menu.js
SECONDARY += node_modules/reveal.js-menu/menu.css
SECONDARY += node_modules/reveal.js-menu/font-awesome/css/all.css
SECONDARY += node_modules/reveal.js-menu/font-awesome/webfonts/fa-solid-900.woff2
SECONDARY += node_modules/reveal.js-menu/font-awesome/webfonts/fa-regular-400.woff2
SECONDARY += node_modules/reveal.js-menu/font-awesome/webfonts/fa-solid-900.woff
SECONDARY += node_modules/reveal.js-menu/font-awesome/webfonts/fa-regular-400.woff
SECONDARY += node_modules/reveal.js-menu/font-awesome/webfonts/fa-solid-900.ttf
SECONDARY += node_modules/reveal.js-menu/font-awesome/webfonts/fa-regular-400.ttf
SECONDARY += node_modules/jquery/dist/jquery.min.js
SECONDARY += $(MATHJAX)

.SECONDARY:  $(SECONDARY)
