#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=19
synth_tb repro2 axis_pkg.vhdl

echo "Test successful"
