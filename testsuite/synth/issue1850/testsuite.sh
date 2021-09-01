#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth pulse.vhdl detector.psl -e > syn_pulse.vhdl

synth issue2.vhdl -e issue > syn_issue2.vhdl

echo "Test successful"
