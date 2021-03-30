#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze_failure e1.vhdl
analyze_failure e2.vhdl

clean

export GHDL_STD_FLAGS=--std=93
analyze_failure e1.vhdl
analyze_failure e2.vhdl

clean

echo "Test successful"
