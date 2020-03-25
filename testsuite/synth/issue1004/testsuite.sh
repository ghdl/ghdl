#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth_analyze test

synth_tb test2

clean

echo "Test successful"
