########### jquery #############
RESOURCES = ../resource/support/jquery.js
jquery/dist/jquery.min.js:
	cd jquery && npm run build

../resource/support/jquery.js: jquery/dist/jquery.min.js
	cp $< $@

########### three.js ###########
RESOURCES += ../resource/support/three.js
../resource/support/three.js: three.js/build/three.min.js
	cp $< $@

########## Chart.js ##########
RESOURCES += ../resource/support/Chart.js
../resource/support/Chart.js: Chart.js/dist/Chart.min.js
	cp $< $@

Chart.js/dist/Chart.min.js:
	cd Chart.js && npm install && npx rollup -c rollup.config.js

########## Chart.js Reveal Plugin ##########
RESOURCES += ../resource/support/csv2chart.js
../resource/support/csv2chart.js: reveal.js-plugins/chart/csv2chart.js
	cp $< $@

########### d3 ###########
RESOURCES += ../resource/support/d3.js
../resource/support/d3.js: d3/dist/d3.min.js
	cp $< $@

d3/dist/d3.min.js: 
	cd d3 && yarn install && yarn run pretest && yarn run rollup -c rollup.config.js

########## MathJax ##########
MATHJAX = MathJax/MathJax.js
MATHJAX += MathJax/config/TeX-AMS_SVG.js
MATHJAX += $(shell find MathJax/jax/input/TeX -name "*.js")
MATHJAX += $(shell find  MathJax/jax/output/SVG -name "*.js")
MATHJAX += $(wildcard MathJax/extensions/*.js)
MATHJAX += $(shell find MathJax/extensions/TeX -name "*.js")
MATHJAX += $(shell find MathJax/jax/element -name "*.js")
RESOURCES += $(MATHJAX:MathJax/%=../resource/support/mathjax/%)

../resource/support/mathjax/%: MathJax/%
	mkdir -p $(@D) && cp $< $@

########## Reveal.js ##########
REVEAL = reveal.js/js/reveal.js
REVEAL += $(shell find reveal.js/css -type f)
REVEAL += $(shell find reveal.js/lib -type f)
REVEAL += $(shell find reveal.js/plugin/math -type f)
REVEAL += $(shell find reveal.js/plugin/zoom-js -type f)
REVEAL += $(shell find reveal.js/plugin/notes -type f)
RESOURCES += $(REVEAL:reveal.js/%=../resource/support/reveal/%)

../resource/support/reveal/%: reveal.js/%
	mkdir -p $(@D) && cp $< $@

########## Reveal.js Menu ###########
REVEAL_MENU = reveal.js-menu/menu.js
REVEAL_MENU += reveal.js-menu/menu.css
REVEAL_MENU += $(shell find reveal.js-menu/font-awesome -type f)
RESOURCES += $(REVEAL_MENU:reveal.js-menu/%=../resource/support/reveal.js-menu/%)
../resource/support/reveal.js-menu/%: reveal.js-menu/%
	mkdir -p $(@D) && cp $< $@

########### bootstrap ###########
RESOURCES += ../resource/support/bootstrap.css
../resource/support/bootstrap.css: bootstrap/dist/css/bootstrap.min.css
	cp $< $@

##########
.PHONY: support
support: $(RESOURCES)