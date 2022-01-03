SHELL := $(shell which bash)

support := ../resource/decker/support/vendor
third := $(shell realpath .)

all: mathjax reveal.js highlight.js fontawesome videojs d3 inert flyingfocus

d3:
	@cp d3.v6.min.js $(support)

videojs:
	@mkdir -p $(support)/videojs
	@cp video.js/* $(support)/videojs

mathjax:
	@mkdir -p $(support)/mathjax/{input,output}
	@for i in tex-svg.js input/tex input/tex.js output/svg output/svg.js; do \
		cp -r $(third)/MathJax/es5/$$i $(support)/mathjax/$$i; \
	done

reveal.js:
	@mkdir -p $(support)/reveal
	@cp -r $(third)/reveal.js/dist $(support)/reveal/
	@cp -r $(third)/reveal.js/plugin $(support)/reveal/

highlight.js:
	@mkdir -p $(support)/css
	@cp $(third)/highlight.js/*.css $(support)/css/

fontawesome:
	@mkdir -p $(support)/fontawesome $(support)/fontawesome/css
	@for i in css/all.css webfonts; do \
		cp -r $(third)/Font-Awesome/js-packages/@fortawesome/fontawesome-free/$$i $(support)/fontawesome/$$i; \
	done

inert: inert.js/dist/inert.min.js
	@mkdir -p $(support)/inert
	@cp $(third)/inert.js/dist/inert.min.js $(support)/inert/
	@cp $(third)/inert.js/dist/inert.min.js.map $(support)/inert/

flyingfocus:
	@mkdir -p $(support)/flying-focus
	@cp $(third)/flying-focus.js/standalone/flying-focus.js $(support)/flying-focus/
	
inert.js/dist/inert.min.js:
	(cd inert.js && npm install && npm run build)

.PHONY: clean prepare fontawesome reveal.js highlight.js mathjax videojs
