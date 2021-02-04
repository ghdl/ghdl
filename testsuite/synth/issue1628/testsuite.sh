#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth_only test
synth_only test2

echo "Test successful"
