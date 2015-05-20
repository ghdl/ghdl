#! /bin/sh

. ../../testenv.sh

analyze_failure bug.vhdl
clean

GHDL_STD_FLAGS=--std=08
analyze bug.vhdl
elab_simulate ent
clean

echo "Test successful"
