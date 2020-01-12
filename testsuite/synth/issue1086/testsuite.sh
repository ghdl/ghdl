#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

synth test.vhdl -e > syn_test.vhdl
clean

echo "Test successful"
