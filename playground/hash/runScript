#!/usr/bin/bash

file=$1
name=$(basename "$file")
ghc -O2 --make "$file" -prof -auto-all -fforce-recomp -rtsopts -threaded
echo $file
./$file +RTS -s -N2
cat $name.hp | hp2ps - | ps2pdf - > hashMultiPar.pdf
mupdf hashMultiPar.pdf &
