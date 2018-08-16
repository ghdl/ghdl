#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze tb.vhdl ram.vhdl fsm.vhdl
elab_simulate_failure testbench

clean

echo "Test successful"
