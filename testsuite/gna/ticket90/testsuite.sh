#! /bin/sh

. ../../testenv.sh

analyze test.vhdl
elab_simulate test
clean

GHDL_STD_FLAGS=--std=08

analyze test.vhdl
analyze test2.vhdl
clean

echo "Test successful"
