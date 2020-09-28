#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze file1.vhdl
analyze_failure file2.vhdl

clean

echo "Test successful"
