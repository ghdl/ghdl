#! /bin/sh

. ../../testenv.sh

analyze_failure testm.vhdl

clean

export GHDL_STD_FLAGS=--std=08
analyze testm.vhdl
elab_simulate testm

clean

echo "Test successful"
