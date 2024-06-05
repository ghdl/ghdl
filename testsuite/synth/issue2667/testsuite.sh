#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth_tb bin_to_7seg

echo "Test successful"
