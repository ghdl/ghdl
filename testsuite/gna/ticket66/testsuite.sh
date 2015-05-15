#! /bin/sh

. ../../testenv.sh

analyze bug.vhdl
elab_simulate ent
clean

GHDL_STD_FLAGS=--std=08
analyze bug.vhdl
elab_simulate ent
clean

echo "Test successful"
