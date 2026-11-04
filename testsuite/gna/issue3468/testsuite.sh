#! /bin/sh

. ../../testenv.sh

#  Reprint shows the delimited comments attached to each node, as written.
"$GHDL" --reprint --std=08 delimited.vhdl > delimited.out
diff delimited.ref delimited.out

rm -f delimited.out

#  Delimited comments are not allowed before vhdl 2008.
analyze_failure --std=93 delimited.vhdl

clean

echo "Test successful"
