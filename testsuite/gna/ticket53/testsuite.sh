#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

analyze decl1.vhdl
analyze ent1.vhdl
elab_simulate ent1

clean

echo "Test successful"
