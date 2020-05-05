#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth_analyze issue1
synth_failure issue2
synth_failure issue3
synth_failure issue4

clean

echo "Test successful"
