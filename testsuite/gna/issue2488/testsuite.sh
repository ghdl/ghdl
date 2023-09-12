#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze compa_pkg.vhdl compa.vhdl top.vhdl tb.vhdl  

elab_simulate_failure tb

clean

echo "Test successful"
