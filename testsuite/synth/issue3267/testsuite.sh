#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=19
synth_only top

echo "Test successful"
