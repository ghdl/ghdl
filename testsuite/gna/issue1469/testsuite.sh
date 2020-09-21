#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze_failure ent.vhdl

analyze ent1.vhdl
elab_simulate ent1

clean

echo "Test successful"
