#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth_only repro

GHDL_STD_FLAGS=--std=93
analyze syn_repro.vhdl

clean

echo "Test successful"
