SVGS := svgs/basic-markdown-deck svgs/extended-markdown-deck svgs/media-deck
SVGDIR := svgs
all: svgdir $(SVGS)

svgdir:
	mkdir -p $(SVGDIR)

svgs/%-deck: public/%-deck.pdf
	pdf2svg $< $@
	pdf2svg $< $@-page-\%03d.svg all

public/%-deck.pdf: public/%-deck.html
	decker pdf

public/%-deck.html: %-deck.md
	decker decks

clean:
	decker purge
	rm -rf $(SVGDIR)

.PHONY : svgdir clean
