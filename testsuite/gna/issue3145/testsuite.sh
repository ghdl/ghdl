#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze ace.vhdl
elab_simulate_failure ace

clean

echo "Test successful"
