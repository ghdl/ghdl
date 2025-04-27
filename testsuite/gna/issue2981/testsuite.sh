#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze tb3.vhdl

analyze tb2.vhdl
elab_simulate tb2

analyze tb.vhdl
elab_simulate function_range

clean

echo "Test successful"
