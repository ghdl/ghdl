#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze_failure type_pkg.vhdl
analyze type_pkg2.vhdl

clean

echo "Test successful"
