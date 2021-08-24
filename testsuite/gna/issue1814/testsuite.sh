#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze ent.vhdl top.vhdl
elab_simulate top

clean

echo "Test successful"
