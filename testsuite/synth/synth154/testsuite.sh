#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth_analyze keep
grep -q "signal a : " syn_keep.vhdl
clean

synth -Werror keep2.vhdl -e > syn_keep2.vhdl
grep -q "signal a : " syn_keep2.vhdl
clean

synth_failure -Werror err1.vhdl -e

echo "Test successful"
