#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze ent2.vhdl
elab_simulate_failure ent2
analyze ent.vhdl
elab_simulate ent

clean

echo "Test successful"
