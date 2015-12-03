#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
GHDL_FLAGS=--work=test

analyze sortnet_OddEvenSort.vhdl
analyze sortnet_OddEvenSort_tb.vhdl
elab_simulate --syn-binding sortnet_OddEvenSort_tb

if false; then
 # Direct instantiation, not yet supported.
 analyze sortnet_BitonicSort.vhdl
 analyze sortnet_BitonicSort_tb.vhdl
 elab_simulate sortnet_BitonicSort_tb
fi

clean test

echo "Test successful"
