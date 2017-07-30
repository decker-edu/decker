#!/bin/sh -e

# Author: Henrik Tramberend <henrik@tramberend.de>

hindent --version
for f in **/*.hs; do
    /bin/echo -n "Check formatting of '$f' ..."
    cat $f | hindent | diff -q - $f || exit
    /bin/echo " OK."
done