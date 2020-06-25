#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze ent.vhdl
elab_simulate test

analyze repro1.vhdl
elab_simulate repro1

clean

echo "Test successful"
