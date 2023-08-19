#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=93
synth_failure ent1.vhdl -e
synth_failure ent2.vhdl -e

echo "Test successful"
