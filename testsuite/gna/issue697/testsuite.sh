#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze_failure test.vhdl

analyze test2.vhdl
elab_simulate test2

clean

echo "Test successful"
