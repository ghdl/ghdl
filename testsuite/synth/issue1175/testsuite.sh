#! /bin/sh

. ../../testenv.sh

synth_analyze issue
clean

GHDL_STD_FLAGS=--std=08
synth_analyze issue
synth_analyze issue2
synth_analyze issue3
clean

echo "Test successful"
