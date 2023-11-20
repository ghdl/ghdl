#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze ent.vhdl
elab_simulate test1

clean

echo "Test successful"
