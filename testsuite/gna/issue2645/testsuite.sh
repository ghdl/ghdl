#! /bin/sh

. ../../testenv.sh

analyze tb.vhdl
elab_simulate example_tb

clean

export GHDL_STD_FLAGS=--std=08
analyze tb.vhdl
elab_simulate example_tb

clean

echo "Test successful"
