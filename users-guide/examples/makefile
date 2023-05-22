SVGS := svgs/text-deck svgs/quote-deck svgs/header-deck svgs/list-deck svgs/image-deck

all: svgdir $(SVGS)

svgdir:
	mkdir -p svgs

svgs/%-deck: public/%-deck.pdf
	pdf2svg $< $@
	pdf2svg $< $@-page-\%03d.svg all

public/%-deck.pdf: public/%-deck.html
	decker pdf

public/%-deck.html: %-deck.md
	decker decks

.PHONY : svgdir