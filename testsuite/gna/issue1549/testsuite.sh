#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze ent.vhdl
elab_simulate ent

analyze ent2.vhdl
elab_simulate ent2

analyze ent3.vhdl
elab_simulate ent3

analyze ent4.vhdl
elab_simulate ent4

analyze ent5.vhdl
elab_simulate ent5

analyze ent6.vhdl
elab_simulate ent6

clean

echo "Test successful"
