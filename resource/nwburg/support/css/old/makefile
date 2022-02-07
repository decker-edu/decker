includes = $(wildcard _*.scss)

css: decker.css page.css handout.css

watch: css
	ls *.scss | entr make css

decker.css: $(includes)
page.css: $(includes)
handout.css: $(includes)

%.css : %.scss
	sassc --style expanded $< $@
