#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=-fsynopsys
analyze std_subs_pkg.vhdl
analyze_failure proc_pkg.vhdl

clean

echo "Test successful"
