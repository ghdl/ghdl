#! /bin/sh

. ../../testenv.sh


GHDL_SYNTH_FLAGS=--latches

synth_tb afed

echo "Test successful"
