#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
$GHDL -i $GHDL_STD_FLAGS ini_pkg.vhdl sr_synchronizer.vhdl map_synchro.vhdl test_9_tb.vhdl
$GHDL -m --expect-failure $GHDL_STD_FLAGS test_9_tb

clean

echo "Test successful"
