#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
GHDL_SYNTH_FLAGS=--latches

synth_tb ent1
synth_only ent

clean
echo "Test successful"
