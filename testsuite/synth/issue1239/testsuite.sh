#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=-fsynopsys
synth repro.vhdl -e > syn_repro.vhdl

synth_failure repro2.vhdl -e
synth_failure repro3.vhdl -e

echo "Test successful"
