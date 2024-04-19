#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze_failure testbench.vhdl

export GHDL_STD_FLAGS="--std=08 -frelaxed"
analyze testbench.vhdl
elab_simulate testbench

clean

echo "Test successful"
