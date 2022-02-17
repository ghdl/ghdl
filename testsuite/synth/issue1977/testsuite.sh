#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth_only triangularcounter

echo "Test successful"
