#!/bin/bash
#
#


HSFLAGS="-prof -O2 -rtsopts -auto-all -caf-all -fobject-code -fforce-recomp -funbox-strict-fields --make"
HSC="ghc"

rm -f *.hi *.hp *.o *.ps *.pdf *.png *.aux *.ps *.eps *.pdf *.png *.prof

for hsf in `ls -1 *.hs | sort -n`; do
    rm -f "${hsf}.bin"
    hsbin=$(echo $hsf | sed 's/.hs/.bin/g')
    rm -f $hsbin
    $HSC $HSFLAGS $hsf -o $hsbin
done

rm -f *.hi *.hp *.o *.ps *.pdf *.png *.aux *.ps *.eps *.pdf *.png *.prof

