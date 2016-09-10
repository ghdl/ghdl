#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

analyze gen1.vhdl
analyze gen2.vhdl
analyze_failure gen3.vhdl
analyze gen4.vhdl

clean

echo "Test successful"
