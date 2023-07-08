#
# creates copies of only the needed files from the third-party repos and resources.
#
SHELL := $(shell which bash)

support := ../resource/decker/support/vendor
third := $(shell realpath .)

upgrade: mathjax reveal.js highlight.js fontawesome videojs d3

d3:
	@rsync d3.v6.min.js $(support)

videojs:
	@mkdir -p $(support)/videojs
	@rsync video.js/* $(support)/videojs

mathjax:
	@mkdir -p $(support)/mathjax/{input,output,input/tex,output/svg}
	@for i in tex-svg.js input/tex/ input/tex.js output/svg/ output/svg.js ui/; do \
		rsync --recursive --delete $(third)/MathJax/es5/$$i $(support)/mathjax/$$i; \
	done

reveal.js:
	@mkdir -p $(support)/reveal
	@rsync --recursive --delete $(third)/reveal.js/dist $(support)/reveal/
	@rsync --recursive --delete $(third)/reveal.js/plugin $(support)/reveal/

highlight.js:
	@mkdir -p $(support)/css
	@rsync $(third)/highlight.js/*.css $(support)/css/

fontawesome:
	@mkdir -p $(support)/fontawesome $(support)/fontawesome/css
	@for i in css/all.css webfonts/; do \
		rsync --recursive --delete $(third)/Font-Awesome/js-packages/@fortawesome/fontawesome-free/$$i $(support)/fontawesome/$$i; \
	done

.PHONY: upgrade clean prepare fontawesome reveal.js highlight.js mathjax videojs
