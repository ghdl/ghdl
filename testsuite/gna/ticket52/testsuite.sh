#! /bin/sh

. ../../testenv.sh

analyze_failure file.vhdl
clean

GHDL_STD_FLAGS=--std=08
analyze file.vhdl
clean

echo "Test successful"
