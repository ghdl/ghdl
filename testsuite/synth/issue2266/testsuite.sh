#! /bin/sh

. ../../testenv.sh

GHDL_FLAGS=--std=08

synth_only reproducer
synth_only repro2

echo "Test successful"
