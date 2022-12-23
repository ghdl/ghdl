#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze_failure repro1.vhdl

$GHDL -s $GHDL_STD_FLAGS repro2.vhdl

analyze tb.vhdl
elab_simulate test_pattern_generator_tb --stop-time=100ns

clean

echo "Test successful"
