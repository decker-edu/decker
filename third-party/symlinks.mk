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


all: jquery mathjax reveal.js water.css lazyload fontawesome thebelab videojs

videojs:
	@mkdir -p $(support)/videojs
	@cp video.js/* $(support)/videojs

thebelab: thebelab/lib/index.js
	@mkdir -p $(support)/thebelab
	@cp thebelab/lib/*.js $(support)/thebelab

jquery: jquery/dist/jquery.min.js
	@mkdir -p $(support)/js
	@cp jquery/dist/jquery.min.js $(support)/js

mathjax:
	@mkdir -p $(support)/mathjax/{input,output}
	@for i in tex-svg.js input/tex input/tex.js output/svg output/svg.js; do \
		$(dup) $(third)/MathJax/es5/$$i $(support)/mathjax/$$i; \
	done

reveal.js:
	@mkdir -p $(support)/reveal/plugin
	@for i in js css plugin/notes plugin/highlight ; do \
		$(dup) $(third)/reveal.js/$$i $(support)/reveal/$$i; \
	done

lazyload:
	@mkdir -p $(support)/js
	@cp $(third)/vanilla-lazyload/dist/lazyload.min.js $(support)/js

water.css:
	@mkdir -p $(support)/css
	@cp $(third)/water.css/dist/light.min.css $(support)/css

fontawesome:
	@mkdir -p $(support)/fontawesome $(support)/fontawesome/css
	@for i in css/all.css webfonts; do \
		$(dup) $(third)/Font-Awesome/js-packages/@fortawesome/fontawesome-free/$$i $(support)/fontawesome/$$i; \
	done

thebelab/lib/index.js:
	(cd thebelab && npm install && npm run build)

jquery/dist/jquery.min.js:
	(cd jquery && npm run build)

.PHONY: clean prepare fontawesome reveal.js water.css lazyload mathjax jquery thebelab videojs
