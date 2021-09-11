#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth test.vhdl test.psl test_sub.vhdl -e test > syn_test.vhdl
synth test.vhdl test.psl test_sub.vhdl -e > syn_test.vhdl

echo "Test successful"
