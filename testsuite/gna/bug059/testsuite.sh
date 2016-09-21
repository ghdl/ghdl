#! /bin/sh

. ../../testenv.sh

analyze_failure tb.vhdl

clean

GHDL_STD_FLAGS=--std=08

# This should fail
analyze tb1.vhdl

clean

echo "Test successful"
