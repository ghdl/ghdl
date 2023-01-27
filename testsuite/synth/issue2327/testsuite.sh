#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

synth_only test_and

synth_tb test_tf

echo "Test successful"
