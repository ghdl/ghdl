#! /bin/sh

. ../../testenv.sh

synth_only mul01
synth_only mul02
synth_only div01

synth_failure divz01.vhdl -e
synth_failure divz02.vhdl -e
synth_failure divz03.vhdl -e
synth_failure expov01.vhdl -e
synth_failure concat01.vhdl -e
synth_failure --std=87 concat02.vhdl -e

GHDL_STD_FLAGS=--std=08
synth_only match01

echo "Test successful"
