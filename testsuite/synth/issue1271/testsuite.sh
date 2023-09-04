#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=93
synth_failure issue.vhdl -e
synth_failure issue2.vhdl -e
synth_failure issue3.vhdl -e

echo "Test successful"
