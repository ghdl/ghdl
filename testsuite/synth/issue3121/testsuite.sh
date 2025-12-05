#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth_failure repro_alias2.vhdl -e

#synth --keep-hierarchy=no repro_proc.vhdl -e > syn_repro_proc.vhdl

echo "Test successful"
