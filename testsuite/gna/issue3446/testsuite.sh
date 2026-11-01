#! /bin/sh

. ../../testenv.sh

#  Reprint shows the comments attached to design units, interfaces and
#  object declarations.
"$GHDL" --reprint --std=19 assoc.vhdl > assoc.out
diff assoc.ref assoc.out

#  A same-line comment is continued by the comments that directly follow it
#  at the same column.
"$GHDL" --reprint --std=19 continuation.vhdl > continuation.out
diff continuation.ref continuation.out

rm -f assoc.out continuation.out

echo "Test successful"
