#! /bin/sh

. ../../testenv.sh

#  The header of the file is attached to the design file, and displayed
#  before the first unit.
"$GHDL" --reprint --std=19 header.vhdl > header.out
diff header.ref header.out

rm -f header.out

echo "Test successful"
