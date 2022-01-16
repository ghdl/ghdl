#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth_only dly1
synth_only dly2
synth_only dly3
synth_only pkg01
synth_only pkg02
synth_only pkg03
synth_only pkg04

echo "Test successful"
