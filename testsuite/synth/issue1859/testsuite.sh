#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth test.vhdl test.psl -e > syn_test.vhdl

echo "Test successful"
