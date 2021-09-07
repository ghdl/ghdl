#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze ent1.vhdl
elab_simulate ent1

analyze ent2.vhdl
elab_simulate ent2

clean

echo "Test successful"
