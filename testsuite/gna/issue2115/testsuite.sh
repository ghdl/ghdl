#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze ent.vhdl
elab_simulate ent

analyze tst08.vhdl
elab_simulate tst08

clean

export GHDL_STD_FLAGS=--std=93
analyze tst93.vhdl
elab_simulate tst93

clean

echo "Test successful"
