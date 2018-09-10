#! /bin/sh

. ../../testenv.sh

analyze_failure test.vhdl
clean

export GHDL_STD_FLAGS=--std=08
analyze_failure test.vhdl
clean

echo "Test successful"
