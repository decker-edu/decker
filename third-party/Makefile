########### jquery #############
RESOURCES = ../resource/support/jquery.js
jquery/dist/jquery.min.js:
	cd jquery && npm run build

../resource/support/jquery.js: jquery/dist/jquery.min.js
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

########## Piklor (dependency of Whiteboard) ##########
RESOURCES += ../resource/support/piklor.js
../resource/support/piklor.js: bielefeld/piklor.js/src/piklor.min.js
	cp $< $@

########## Whiteboard ##########
RESOURCES += ../resource/support/whiteboard.js
RESOURCES += ../resource/support/sponge.png
../resource/support/whiteboard.js: bielefeld/mb-plugins/whiteboard/whiteboard.js
	cp $< $@
../resource/support/sponge.png: ../src-support/img/sponge.png
	cp $< $@

########## Fontawesome ##########
FONTAWESOME += Font-Awesome/js-packages/@fortawesome/fontawesome-free/js/all.js
FONTAWESOME += Font-Awesome/js-packages/@fortawesome/fontawesome-free/css/all.css
FONTAWESOME += $(shell find Font-Awesome/js-packages/@fortawesome/fontawesome-free/webfonts -type f)
FONTAWESOME += $(shell find Font-Awesome/js-packages/@fortawesome/fontawesome-free/svgs -type f)
FONTAWESOME += $(shell find Font-Awesome/js-packages/@fortawesome/fontawesome-free/sprites -type f)
RESOURCES += $(FONTAWESOME:Font-Awesome/js-packages/@fortawesome/fontawesome-free/%=../resource/support/fontawesome/%)

../resource/support/fontawesome/js/all.js: Font-Awesome/js-packages/@fortawesome/fontawesome-free/js/all.min.js
	mkdir -p $(@D) && cp $< $@

../resource/support/fontawesome/css/all.css: Font-Awesome/js-packages/@fortawesome/fontawesome-free/css/all.min.css
	mkdir -p $(@D) && cp $< $@

../resource/support/fontawesome/webfonts/%: Font-Awesome/js-packages/@fortawesome/fontawesome-free/webfonts/%
	mkdir -p $(@D) && cp $< $@

../resource/support/fontawesome/svgs/%: Font-Awesome/js-packages/@fortawesome/fontawesome-free/svgs/%
	mkdir -p $(@D) && cp $< $@

../resource/support/fontawesome/sprites/%: Font-Awesome/js-packages/@fortawesome/fontawesome-free/sprites/%
	mkdir -p $(@D) && cp $< $@
	
##########
.PHONY: support
support: $(RESOURCES)