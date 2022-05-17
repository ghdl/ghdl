#! /bin/sh

. ../../testenv.sh

synth_only repro

GHDL_STD_FLAGS=--std=08
synth_only fxt2

echo "Test successful"
