guide-pdf := decker-users-guide.pdf
guide-html := decker-users-guide.html
guide-md := decker-users-guide.md

$(guide-html): $(guide-md)
	pandoc --standalone --self-contained --toc --toc-depth=2 -o $@ $<

$(guide-pdf): $(guide-md)
	pandoc --standalone --toc --toc-depth=2 -o $@ $<

watch: $(guide-html) $(guide-pdf)
	ls $(guide-md) | entr make $^ 

