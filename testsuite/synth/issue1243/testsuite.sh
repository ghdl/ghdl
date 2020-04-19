#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth_failure DSPn.vhdl -e
synth_failure issue.vhdl -e

echo "Test successful"
