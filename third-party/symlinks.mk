# Copy things that need to built first (JQuery, Chart.js). The rest is symlinked
# directly into the corresponding repository.

SHELL := $(shell which bash)

support := ../resource/decker/support/vendor
third := $(shell realpath .)

ifeq ($(OS),Windows_NT)
dup = cp -r 
else
dup = ln -sF
endif

ifeq ($(copy), true)
dup = cp -r
endif


all: mathjax reveal.js water.css highlight.js fontawesome thebelab videojs d3

d3:
	@cp d3.v6.min.js $(support)

videojs:
	@mkdir -p $(support)/videojs
	@cp video.js/* $(support)/videojs

thebelab: thebelab/lib/index.js
	@mkdir -p $(support)/thebelab
	@cp thebelab/lib/*.js $(support)/thebelab

mathjax:
	@mkdir -p $(support)/mathjax/{input,output}
	@for i in tex-svg.js input/tex input/tex.js output/svg output/svg.js; do \
		$(dup) $(third)/MathJax/es5/$$i $(support)/mathjax/$$i; \
	done

reveal.js:
	@mkdir -p $(support)/reveal
	@$(dup) $(third)/reveal.js/dist $(support)/reveal/
	@$(dup) $(third)/reveal.js/plugin $(support)/reveal/

water.css:
	@mkdir -p $(support)/css
	@cp $(third)/water.css/dist/*.min.css $(support)/css
	@chmod a-x $(support)/css/*

highlight.js:
	@mkdir -p $(support)/css
	@cp $(third)/highlight.js/*.css $(support)/css/

fontawesome:
	@mkdir -p $(support)/fontawesome $(support)/fontawesome/css
	@for i in css/all.css webfonts; do \
		$(dup) $(third)/Font-Awesome/js-packages/@fortawesome/fontawesome-free/$$i $(support)/fontawesome/$$i; \
	done

thebelab/lib/index.js:
	@(cd thebelab && npm install && npm run build)

.PHONY: clean prepare fontawesome reveal.js water.css highlight.js mathjax thebelab videojs
