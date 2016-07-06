#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
analyze repro.vhdl
analyze test.vhdl
elab_simulate test

clean

echo "Test successful"
