#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

synth test.vhdl -e test > syn_test.vhdl
analyze syn_test.vhdl
clean

echo "Test successful"
