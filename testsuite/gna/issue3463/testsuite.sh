#! /bin/sh

. ../../testenv.sh

#  A comment on the line of the header of a block construct is attached to the
#  construct: reprint displays it before the construct.
"$GHDL" --reprint --std=19 header.vhdl > header.out
diff header.ref header.out

rm -f header.out

echo "Test successful"
