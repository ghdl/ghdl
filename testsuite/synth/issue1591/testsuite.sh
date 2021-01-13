#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
synth_analyze issue
synth_analyze repro1
synth_analyze repro2
synth_analyze repro3
synth_analyze repro4

clean
echo "Test successful"
