#! /bin/sh

. ../../testenv.sh

analyze adder.vhdl
analyze adder_tb.vhdl
elab_simulate adder_tb

analyze_failure adder_tb2.vhdl

clean

echo "Test successful"
