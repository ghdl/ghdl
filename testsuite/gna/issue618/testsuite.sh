#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze e.vhdl
elab_simulate e
analyze e1.vhdl
elab_simulate_failure e1

clean

echo "Test successful"
