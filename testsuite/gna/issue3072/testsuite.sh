#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze_failure test.vhdl
analyze_failure test1.vhdl
analyze_failure test2.vhdl
analyze_failure test2b.vhdl

clean

echo "Test successful"
