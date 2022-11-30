#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

synth_only test
analyze syn_test.vhdl

synth_only func

synth_only snot
clean

echo "Test successful"
