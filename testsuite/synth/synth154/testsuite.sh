#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth_analyze keep
grep -q "signal a : " syn_keep.vhdl
clean

echo "Test successful"
