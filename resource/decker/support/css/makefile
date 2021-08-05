includes = $(wildcard _*.scss)

css: deck.css

watch: css
	ls *.scss | entr make css

deck.css: $(includes)

%.css : %.scss
	sassc --style expanded $< $@
