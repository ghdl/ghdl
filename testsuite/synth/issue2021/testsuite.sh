#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth_only repro1
synth_only repro2
synth_only ent

echo "Test successful"
