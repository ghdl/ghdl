#! /bin/sh

. ../../testenv.sh

synth_only repro
synth_only repro2

#GHDL_STD_FLAGS=--std=08
#synth_only divider

echo "Test successful"
