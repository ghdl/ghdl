#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze pkg0.vhdl
analyze pkg1.vhdl
analyze pkg2.vhdl
analyze pkg3.vhdl
analyze_failure pkg4.vhdl
analyze pkg5.vhdl
analyze_failure pkg6.vhdl

analyze tb.vhdl
elab_simulate tb
clean

echo "Test successful"
