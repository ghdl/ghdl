#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth ent.vhdl -e > syn_ent.vhdl
analyze syn_ent.vhdl
clean

echo "Test successful"
