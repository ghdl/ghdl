#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze testbench.vhdl
elab_simulate testbench

clean

echo "Test successful"
