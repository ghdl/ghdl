#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze repro1.vhdl
elab_simulate test_entity

clean

analyze repro2.vhdl
elab_simulate test_entity

clean

analyze ent.vhdl
elab_simulate ent

clean

echo "Test successful"
