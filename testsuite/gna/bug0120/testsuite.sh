#! /bin/sh

. ../../testenv.sh

$GHDL fmt --std=08 --level=space print1.vhdl > print1.out
ghdl_diff_stcr print1.ref print1.out

$GHDL fmt --std=08 --range=10:10 print1.vhdl > print2.out
ghdl_diff_stcr print2.ref print2.out

echo "Test successful"
