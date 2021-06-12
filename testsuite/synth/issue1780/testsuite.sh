#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

synth_only imem2

echo "Test successful"
