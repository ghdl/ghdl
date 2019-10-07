#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

analyze test2.vhdl
synth test2 > syn_test2.vhdl
clean

echo "Test successful"
