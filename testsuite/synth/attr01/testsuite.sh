#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth_analyze attr01
grep -q "keep of counter" syn_attr01.vhdl
clean

synth_analyze attr02
grep -q "keep of rst" syn_attr02.vhdl
clean

echo "Test successful"
