#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze_failure obj.vhdl
analyze_failure type.vhdl

clean

echo "Test successful"
