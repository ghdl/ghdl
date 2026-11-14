#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

analyze compa_pkg.vhdl compa.vhdl top.vhdl tb.vhdl
elab_simulate tb --stop-time=1us

clean

echo "Test successful"
