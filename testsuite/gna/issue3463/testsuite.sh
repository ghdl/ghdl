#! /bin/sh

. ../../testenv.sh

#  A comment on the line of the header of a block construct is attached to the
#  construct: reprint displays it before the construct.
"$GHDL" --reprint --std=19 header.vhdl > header.out
diff header.ref header.out

#  So is a comment on the line of its end.
"$GHDL" --reprint --std=19 end.vhdl > end.out
diff end.ref end.out

rm -f header.out end.out

echo "Test successful"
