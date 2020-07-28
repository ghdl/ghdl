#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS="--std=08 --work=test"

analyze repro.vhdl
analyze repro2.vhdl
elab_simulate repro2

analyze sortnet_OddEvenSort.vhdl
analyze sortnet_OddEvenSort_tb.vhdl
elab_simulate --syn-binding sortnet_OddEvenSort_tb

# Direct instantiation, not yet supported.
analyze sortnet_BitonicSort.vhdl
analyze sortnet_BitonicSort_tb.vhdl
elab_simulate sortnet_BitonicSort_tb

clean test

echo "Test successful"
