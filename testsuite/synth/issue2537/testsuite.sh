#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
synth_only shifter

echo "Test successful"
