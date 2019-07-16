#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze_failure alt2.vhdl
analyze alt.vhdl

clean

echo "Test successful"
