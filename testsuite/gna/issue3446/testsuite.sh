#! /bin/sh

. ../../testenv.sh

#  Reprint shows the comments attached to design units, interfaces and
#  object declarations.
"$GHDL" --reprint --std=19 assoc.vhdl > assoc.out
diff assoc.ref assoc.out

rm -f assoc.out

echo "Test successful"
