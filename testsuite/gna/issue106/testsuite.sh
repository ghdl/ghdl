#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

analyze_failure ent.vhdl

analyze ent1.vhdl
elab_simulate ent

analyze ent2.vhdl
elab_simulate ent

analyze case1.vhdl
elab_simulate ent

clean

echo "Test successful"
