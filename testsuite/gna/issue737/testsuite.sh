#! /bin/sh

#exit 0
. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

analyze ent.vhdl
elab_simulate ent

analyze ent1.vhdl
elab_simulate ent1

analyze ent2.vhdl
elab_simulate ent2

analyze ent3.vhdl
elab_simulate ent3

analyze ent4.vhdl
elab_simulate ent4

clean

echo "Test successful"
