#! /bin/sh

#exit 0
. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze test-bug.vhdl
elab_simulate test_arr

clean

echo "Test successful"
