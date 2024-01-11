#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze repro.vhdl
elab_simulate repro

clean

analyze test.vhdl
clean

echo "Test successful"
