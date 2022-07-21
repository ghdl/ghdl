#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze repro1.vhdl
elab_simulate repro1

analyze test.vhdl
elab_simulate test

clean

echo "Test successful"
