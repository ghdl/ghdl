#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth pkg.vhdl foo.vhdl -e > syn_foo.vhdl
analyze pkg.vhdl syn_foo.vhdl
clean

echo "Test successful"
