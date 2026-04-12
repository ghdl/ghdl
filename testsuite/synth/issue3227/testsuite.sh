#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=19
synth_tb repro2 axis_pkg.vhdl
fgrep -q '\m[ready]\ <= \s[0][ready]\;' syn_repro2.vhdl

synth_tb repro1 axis_pkg.vhdl

echo "Test successful"
