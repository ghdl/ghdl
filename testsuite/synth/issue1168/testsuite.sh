#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth_analyze bug
synth_analyze bug2

clean

echo "Test successful"
