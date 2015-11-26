#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
GHDL_FLAGS=--work=test

analyze sortnet_OddEvenSort.vhdl
analyze sortnet_OddEvenSort_tb.vhdl
elab_simulate --syn-binding sortnet_OddEvenSort_tb

clean test

echo "Test successful"
