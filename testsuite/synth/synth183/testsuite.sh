#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

synth_analyze test

clean

echo "Test successful"
