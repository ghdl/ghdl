#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze test.vhdl test.psl
$GHDL -m $GHDL_STD_FLAGS test

clean

echo "Test successful"
