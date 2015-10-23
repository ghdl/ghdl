#! /bin/sh

. ../../testenv.sh

"$GHDL" --lines adder.vhdl

echo "Test successful"
