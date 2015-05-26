#! /bin/sh

. ../../testenv.sh

analyze repro.vhdl
analyze repro2.vhdl
clean

GHDL_STD_FLAGS=--std=08
analyze bug.vhdl
elab_simulate ent
clean

echo "Test successful"
