#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

synth_only crc-orig
#synth_only crc
synth_only repro1
synth_only repro2

echo "Test successful"
