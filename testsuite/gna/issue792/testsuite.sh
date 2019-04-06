#! /bin/sh

. ../../testenv.sh

analyze_failure dut.vhdl

clean

export GHDL_STD_FLAGS=--std=08
analyze_failure dut.vhdl

clean

echo "Test successful"
