#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth_analyze repro1
synth_analyze fixed_point_example
clean

echo "Test successful"
