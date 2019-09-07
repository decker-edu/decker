support := ../resource/support/vendor

default: support

# jquery
RESOURCES = $(support)/jquery.js
jquery/dist/jquery.min.js:
	cd jquery && npm run build

$(support)/jquery.js: jquery/dist/jquery.min.js
	mkdir -p $(@D) && cp $< $@

# Chart.js
RESOURCES += $(support)/Chart.js
$(support)/Chart.js: Chart.js/dist/Chart.min.js
	mkdir -p $(@D) && cp $< $@

Chart.js/dist/Chart.min.js:
	cd Chart.js && npm install && npx rollup -c rollup.config.js

# Chart.js Reveal Plugin
RESOURCES += $(support)/csv2chart.js
$(support)/csv2chart.js: reveal.js-plugins/chart/csv2chart.js
	mkdir -p $(@D) && cp $< $@

# MathJax
MATHJAX = MathJax/MathJax.js
MATHJAX += MathJax/config/TeX-AMS_SVG.js
MATHJAX += $(shell find MathJax/jax/input/TeX -name "*.js")
MATHJAX += $(shell find  MathJax/jax/output/SVG -name "*.js")
MATHJAX += $(wildcard MathJax/extensions/*.js)
MATHJAX += $(shell find MathJax/extensions/TeX -name "*.js")
MATHJAX += $(shell find MathJax/jax/element -name "*.js")
RESOURCES += $(MATHJAX:MathJax/%=$(support)/mathjax/%)

$(support)/mathjax/%: MathJax/%
	mkdir -p $(@D) && cp $< $@

# Reveal.js
REVEAL = reveal.js/js/reveal.js
REVEAL += $(shell find reveal.js/css -type f)
REVEAL += $(shell find reveal.js/lib -type f)
REVEAL += $(shell find reveal.js/plugin/math -type f)
REVEAL += $(shell find reveal.js/plugin/zoom-js -type f)
REVEAL += $(shell find reveal.js/plugin/notes -type f)
RESOURCES += $(REVEAL:reveal.js/%=$(support)/reveal/%)

$(support)/reveal/%: reveal.js/%
	mkdir -p $(@D) && cp $< $@

# Reveal.js Menu
REVEAL_MENU = reveal.js-menu/menu.js
REVEAL_MENU += reveal.js-menu/menu.css
REVEAL_MENU += $(shell find reveal.js-menu/font-awesome -type f)
RESOURCES += $(REVEAL_MENU:reveal.js-menu/%=$(support)/reveal.js-menu/%)
$(support)/reveal.js-menu/%: reveal.js-menu/%
	mkdir -p $(@D) && cp $< $@

# bootstrap
RESOURCES += $(support)/bootstrap.min.css
$(support)/bootstrap.min.css: bootstrap/dist/css/bootstrap.min.css
	mkdir -p $(@D) && cp $< $@

RESOURCES += $(support)/bootstrap.min.css.map
$(support)/bootstrap.min.css.map: bootstrap/dist/css/bootstrap.min.css.map
	mkdir -p $(@D) && cp $< $@

# Piklor (dependency of Whiteboard)
RESOURCES += $(support)/piklor.js
$(support)/piklor.js: piklor.js/src/piklor.min.js
	mkdir -p $(@D) && cp $< $@
	echo "\n\nPIKLOR\n\n"

# Mario's plugins
RESOURCES += $(support)/mb-reveal-plugins
$(support)/mb-reveal-plugins: mb-reveal-plugins
	cp -r mb-reveal-plugins $(support)/
	echo "\n\nMARIO\n\n"

# Fontawesome
FONTAWESOME += Font-Awesome/js-packages/@fortawesome/fontawesome-free/js/all.js
FONTAWESOME += Font-Awesome/js-packages/@fortawesome/fontawesome-free/css/all.css
FONTAWESOME += $(shell find Font-Awesome/js-packages/@fortawesome/fontawesome-free/webfonts -type f)
FONTAWESOME += $(shell find Font-Awesome/js-packages/@fortawesome/fontawesome-free/svgs -type f)
FONTAWESOME += $(shell find Font-Awesome/js-packages/@fortawesome/fontawesome-free/sprites -type f)
RESOURCES += $(FONTAWESOME:Font-Awesome/js-packages/@fortawesome/fontawesome-free/%=$(support)/fontawesome/%)

$(support)/fontawesome/js/all.js: Font-Awesome/js-packages/@fortawesome/fontawesome-free/js/all.min.js
	mkdir -p $(@D) && cp $< $@

$(support)/fontawesome/css/all.css: Font-Awesome/js-packages/@fortawesome/fontawesome-free/css/all.min.css
	mkdir -p $(@D) && cp $< $@

$(support)/fontawesome/webfonts/%: Font-Awesome/js-packages/@fortawesome/fontawesome-free/webfonts/%
	mkdir -p $(@D) && cp $< $@

$(support)/fontawesome/svgs/%: Font-Awesome/js-packages/@fortawesome/fontawesome-free/svgs/%
	mkdir -p $(@D) && cp $< $@

$(support)/fontawesome/sprites/%: Font-Awesome/js-packages/@fortawesome/fontawesome-free/sprites/%
	mkdir -p $(@D) && cp $< $@

support: $(RESOURCES)

.PHONY: support
