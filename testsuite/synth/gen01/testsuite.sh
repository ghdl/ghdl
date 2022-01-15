#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth_only dly1
synth_only dly2
synth_only dly3

echo "Test successful"
