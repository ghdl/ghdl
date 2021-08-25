#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth_analyze regfile
grep -q "variable registers :" syn_regfile.vhdl
clean

echo "Test successful"
