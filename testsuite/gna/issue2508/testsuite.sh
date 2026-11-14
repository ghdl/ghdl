#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze top.vhdl tb.vhdl
elab_simulate_failure tb

clean

echo "Test successful"
