#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth_only repro
synth_only ent

echo "Test successful"
