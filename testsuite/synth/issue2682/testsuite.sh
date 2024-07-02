#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth_only ent
synth_only entr

echo "Test successful"
