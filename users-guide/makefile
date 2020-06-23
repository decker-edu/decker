guide-pdf := decker-users-guide.pdf
guide-html := decker-users-guide.html
guide-md := decker-users-guide.md

all: $(guide-html) $(guide-pdf)

$(guide-html): $(guide-md) makefile
	pandoc -f markdown+emoji --standalone --self-contained --toc --toc-depth=2 -o $@ $<

$(guide-pdf): $(guide-md) makefile
	pandoc --pdf-engine=xelatex -M mainfont=Georgia --standalone --toc --toc-depth=2 -o $@ $<

watch: $(guide-html)
	open -a safari $(guide-html)
	ls $(guide-md) makefile | entr make $^ 

