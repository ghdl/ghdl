#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze timing_pkg.vhdl
analyze_failure tb_to_string_overloading.vhdl 

clean

analyze timing_pkg2.vhdl
analyze_failure tb_to_string_overloading.vhdl 

clean

echo "Test successful"
