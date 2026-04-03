#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

synth_only repro2

echo "Test successful"
