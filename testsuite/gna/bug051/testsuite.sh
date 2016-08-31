#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
analyze_failure tb1.vhdl
analyze_failure -fpsl tb2.vhdl
analyze tb.vhdl
elab_simulate tb

clean

echo "Test successful"
