#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

analyze gen1.vhdl
analyze gen2.vhdl
analyze_failure pkg1.vhdl

analyze gen1-body.vhdl
analyze pkg1.vhdl

clean

echo "Test successful"
