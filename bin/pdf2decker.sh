#!/bin/bash


# input and output file(s)
IN=$1
OUT=${1/.pdf/-deck.md}
PAGES=./${1/.pdf/-pages}


# extract width and height from metadata
XRES=`pdfinfo $IN | grep "Page size" | awk '{print $3}'`
YRES=`pdfinfo $IN | grep "Page size" | awk '{print $5}'`


# extract title from metadata
TITLE=`pdfinfo $IN | grep "Title:" | awk '{$1=""}1'`


echo "Convert" $IN "to" $OUT "with resolution" $XRES "x" $YRES


# convert PDF pages to SVG images
rm -rf $PAGES
mkdir $PAGES
pdf2svg $IN $PAGES/page-%03d.svg all


# YAML header
echo "---" > $OUT
echo "title: " $TITLE >> $OUT
echo "author: Me, myself, and I" >> $OUT
echo "width: " $XRES >> $OUT
echo "height:" $YRES >> $OUT
echo "..." >> $OUT
echo >> $OUT


# add pages as full-width images
for page in $PAGES/page*.svg
do 
    echo '![]('$page'){width=100%}' >> $OUT
    echo >> $OUT
    echo '---' >> $OUT
    echo >> $OUT
done


echo "done"
