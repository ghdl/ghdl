#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

synth_only top2
synth_only top

echo "Test successful"
