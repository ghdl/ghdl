#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze_failure -Werror repro.vhdl

analyze repro.vhdl
elab_simulate_failure repro

clean

echo "Test successful"
