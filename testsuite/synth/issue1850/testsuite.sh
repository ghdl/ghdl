#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth pulse.vhdl detector.psl -e > syn_pulse.vhdl

echo "Test successful"
