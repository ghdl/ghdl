#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth_only test
grep 1100 syn_test.vhdl

echo "Test successful"
