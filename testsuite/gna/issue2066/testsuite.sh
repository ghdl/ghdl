#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze repro1.vhdl
elab_simulate_failure repro1

analyze aggregate_bug.vhdl
elab_simulate_failure aggregate_bug

clean

echo "Test successful"
