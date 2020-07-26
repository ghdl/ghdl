#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth_analyze repro
synth_analyze fixed_point_example
clean

echo "Test successful"
