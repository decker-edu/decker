#!/bin/sh

# Author: Henrik Tramberend <henrik@tramberend.de>

for f in **/*.hs; do
    cat $f | hindent | diff -q - $f || exit
done