#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze tb-orig.vhdl
elab_simulate testbench --stop-time=100ns

clean

echo "Test successful"
