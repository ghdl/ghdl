#! /bin/sh

. ../../testenv.sh

analyze repro.vhdl
elab_simulate repro

clean

export GHDL_STD_FLAGS=--std=08
analyze repro.vhdl
elab_simulate repro

clean

echo "Test successful"
