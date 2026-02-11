#! /bin/sh

. ../../testenv.sh

synth_only t2

GHDL_STD_FLAGS=--std=08
synth_only t

echo "Test successful"
