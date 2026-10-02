#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze ent.vhdl
elab_simulate ent

analyze repro.vhdl
elab_simulate repro

clean

echo "Test successful"
