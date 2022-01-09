#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze test_pkg.vhdl
analyze repro2.vhdl
elab_simulate repro2

clean

echo "Test successful"
