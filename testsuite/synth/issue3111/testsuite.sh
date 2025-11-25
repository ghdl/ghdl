#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth --keep-hierarchy=no repro_for.vhdl -e > syn_repro_for.vhdl

synth --keep-hierarchy=no repro_if.vhdl -e > syn_repro_if.vhdl

echo "Test successful"
