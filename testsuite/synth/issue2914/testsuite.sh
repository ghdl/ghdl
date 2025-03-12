#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
GHDL_SYNTH_FLAGS=--keep-hierarchy=no

synth module.vhdl vunit.psl -e > syn_module.vhdl
analyze syn_module.vhdl

clean

echo "Test successful"
