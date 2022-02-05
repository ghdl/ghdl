#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth_only bug
synth_only bug2

echo "Test successful"
