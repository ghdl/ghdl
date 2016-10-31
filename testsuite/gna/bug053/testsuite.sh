#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

analyze_failure tb1.vhdl

analyze tb2.vhdl
elab_simulate tb2

analyze tb3.vhdl
elab_simulate tb3

clean

echo "Test successful"
