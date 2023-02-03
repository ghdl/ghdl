#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth_failure top_ecp5pll.vhd ecp5pll.vhd  -e top_ecp5pll

echo "Test successful"
