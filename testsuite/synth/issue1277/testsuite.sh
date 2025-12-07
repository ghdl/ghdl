#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

synth_analyze issue
synth_analyze issue2
clean

echo "Test successful"
