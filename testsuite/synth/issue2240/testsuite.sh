#! /bin/sh

. ../../testenv.sh

synth_only case_concat

GHDL_STD_FLAGS=--std=08
synth_only case_concat

echo "Test successful"
