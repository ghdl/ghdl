#! /bin/sh

. ../../testenv.sh

# Use force-analysis to keep parentheses"
GHDL_STD_FLAGS="--std=08 --force-analysis"

synth_only repro1
synth_only repro2
synth_only repro3


echo "Test successful"
