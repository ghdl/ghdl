#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze_failure top.vhdl
analyze_failure top2.vhdl

clean

echo "Test successful"
