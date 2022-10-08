#! /bin/sh

. ../../testenv.sh

"$GHDL" --reprint e.vhdl > reprint_e.vhdl

echo "Test successful"
