# Copy things that need to built first (JQuery, Chart.js). The rest is symlinked
# directly into the corresponding repository.

SHELL := $(shell which bash)

support := ../resource/support/vendor
third := $(shell realpath .)

ifeq ($(OS),Windows_NT)
dup = cp -r 
else
dup = ln -sF
endif

ifeq ($(copy), true)
dup = cp -r
endif


all: jquery Chart.js mathjax reveal.js bootstrap piklor.js fontawesome reveal.js-menu thebelab

thebelab: thebelab/lib/index.js
	@mkdir -p $(support)/thebelab
	@cp thebelab/lib/*.{js,map} $(support)/thebelab

jquery: jquery/dist/jquery.min.js
	@mkdir -p $(support)
	@cp jquery/dist/jquery.min.js $(support)/jquery.js

mathjax:
	@mkdir -p $(support)/mathjax/{input,output}
	@for i in tex-svg.js input/tex input/tex.js output/svg output/svg.js; do \
		$(dup) $(third)/MathJax/es5/$$i $(support)/mathjax/$$i; \
	done

reveal.js:
	@mkdir -p $(support)/reveal/plugin $(support)/reveal/plugin/markdown $(support)/reveal/plugin/markdown
	@cp $(third)/reveal.js/plugin/markdown/marked.js $(support)/reveal/plugin/markdown/marked.js
	@for i in js css lib plugin/math plugin/zoom-js plugin/notes; do \
		$(dup) $(third)/reveal.js/$$i $(support)/reveal/$$i; \
	done

reveal.js-menu:
	@mkdir -p $(support)/reveal.js-menu
	@for i in menu.css menu.js; do \
		$(dup) $(third)/reveal.js-menu/$$i $(support)/reveal.js-menu/$$i; \
	done

bootstrap:
	@mkdir -p $(support)/bootstrap
	@$(dup) $(third)/bootstrap/dist/css $(support)/bootstrap/css 

piklor.js:
	@$(dup) $(third)/piklor.js/src/piklor.min.js $(support)/piklor.js

fontawesome:
	@mkdir -p $(support)/fontawesome
	@for i in js css webfonts svgs sprites; do \
		$(dup) $(third)/Font-Awesome/js-packages/@fortawesome/fontawesome-free/$$i $(support)/fontawesome/$$i; \
	done

thebelab/lib/index.js:
	(cd thebelab && npm install && npm run build)

jquery/dist/jquery.min.js:
	(cd jquery && npm run build)

Chart.js/dist/Chart.min.js:
	(cd Chart.js && npm install && npx rollup -c rollup.config.js)

.PHONY: clean prepare fontawesome piklor.js bootstrap reveal.js mathjax jquery reveal.js-menu thebelab 
