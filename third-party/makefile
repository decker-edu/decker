#
# creates copies of only the needed files from the third-party repos and resources.
#
SHELL := $(shell which bash)

support := ../resource/decker/support/vendor
third := $(shell realpath .)

upgrade: mathjax reveal.js highlight.js fontawesome videojs d3 chartjs codapi-js codejar highlight

d3:
	@rsync d3.v6.min.js $(support)

videojs:
	@mkdir -p $(support)/videojs
	@rsync video.js/* $(support)/videojs

mathjax:
	@mkdir -p $(support)/mathjax/{input,output,input/tex,output/svg,a11y,sre}
	@for i in tex-svg.js a11y/ sre/ input/tex/ input/tex.js input/mml.js output/ ui/; do \
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

chartjs:
	@mkdir -p $(support)/chartjs
	@rsync chart.js/dist/chart.umd.js $(support)/chartjs/

codapi-js:
	@(cd codapi-js; npm install; make)
	@mkdir -p $(support)/codapi-js
	@rsync -r codapi-js/dist/* $(support)/codapi-js

codejar:
	@(cd codejar; npm run build)
	@mkdir -p $(support)/codejar
	@rsync -r codejar/dist/* $(support)/codejar

highlight:
	@mkdir -p $(support)/highlight
	@rsync -r highlight/* $(support)/highlight

.PHONY: upgrade clean prepare fontawesome reveal.js highlight.js mathjax videojs chartjs codapi-js codejar highlight
