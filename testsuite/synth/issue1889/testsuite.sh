#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

synth shiftmux2.vhdl shiftmux2.psl -e > syn_shiftmux2.vhdl

echo "Test successful"
