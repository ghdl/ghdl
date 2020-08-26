#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
synth_analyze fixed_round_crash_correct
synth_failure fixed_round_crash_incorrect
clean

echo "Test successful"
