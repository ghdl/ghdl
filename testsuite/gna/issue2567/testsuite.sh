#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=-Werror
analyze_failure e.vhdl
analyze_failure e2.vhdl
analyze_failure e3.vhdl
analyze_failure e4.vhdl
analyze_failure e5.vhdl
analyze_failure e6.vhdl

clean

echo "Test successful"
