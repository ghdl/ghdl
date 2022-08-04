#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
synth_only bug

echo "Test successful"
