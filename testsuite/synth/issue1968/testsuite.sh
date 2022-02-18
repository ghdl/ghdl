#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth -fsynopsys dummy_pkg.vhdl dummy.vhdl -e > syn_dummy.vhdl

echo "Test successful"
