#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

synth generic_type_prev.vhdl generic_type_prev_tb.vhdl -e > syn_generic.vhdl
synth sl_prev.vhdl generic_type_prev_tb.vhdl -e > syn_sl.vhdl
synth workaround_prev.vhdl generic_type_prev_tb.vhdl -e > syn_workaround.vhdl

echo "Test successful"
