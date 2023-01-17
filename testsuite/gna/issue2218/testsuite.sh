#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze_failure test2.vhdl

analyze test.vhdl
elab_simulate test

clean

echo "Test successful"
