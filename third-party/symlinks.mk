# Copy things that need to built first (JQuery, Chart.js). The rest is symlinked
# directly into the corresponding repository.

SHELL := $(shell which bash)

support := ../resource/support/vendor
third := $(shell realpath .)

all: jquery chart.js mathjax reveal.js bootstrap piklor.js whiteboard fontawesome reveal.js-menu

jquery: jquery/dist/jquery.min.js
	@cp jquery/dist/jquery.min.js $(support)/jquery.js

chart.js: Chart.js/dist/Chart.min.js 
	@cp Chart.js/dist/Chart.min.js $(support)/Chart.js
	@ln -sf $(third)/reveal.js-plugins/chart/csv2chart.js $(support)/csv2chart.js

mathjax:
	@mkdir -p $(support)/mathjax/{jax/input,jax/output}
	@for i in MathJax.js config jax/input/TeX jax/output/SVG jax/element extensions; do \
		ln -sF $(third)/MathJax/$$i $(support)/mathjax/$$i; \
	done

reveal.js:
	@mkdir -p $(support)/reveal/plugin
	@for i in js css lib plugin/math plugin/zoom-js plugin/notes; do \
		ln -sF $(third)/reveal.js/$$i $(support)/reveal/$$i; \
	done

reveal.js-menu:
	@mkdir -p $(support)/reveal.js-menu
	@for i in menu.css menu.js; do \
		ln -sF $(third)/reveal.js-menu/$$i $(support)/reveal.js-menu/$$i; \
	done

bootstrap:
	@mkdir -p $(support)/bootstrap
	@ln -sF $(third)/bootstrap/dist/css $(support)/bootstrap/css 

piklor.js:
	@ln -sf $(third)/piklor.js/src/piklor.min.js $(support)/piklor.js

whiteboard:
	@ln -sF $(third)/mb-reveal-plugins/whiteboard $(support)/whiteboard

fontawesome:
	@mkdir -p $(support)/fontawesome
	@for i in js css webfonts svgs sprites; do \
		ln -sF $(third)/Font-Awesome/js-packages/@fortawesome/fontawesome-free/$$i $(support)/fontawesome/$$i; \
	done

prepare: clean
	@mkdir -p $(support)

clean:
	@rm -rf $(support)

jquery/dist/jquery.min.js:
	cd jquery && npm run build

Chart.js/dist/Chart.min.js:
	cd Chart.js && npm install && npx rollup -c rollup.config.js

.PHONY: clean fontawesome whiteboard piklor.js bootstrap reveal.js mathjax chart.js jquery reveal.js-menu
