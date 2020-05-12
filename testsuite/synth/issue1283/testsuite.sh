#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth_analyze issue1
synth_failure issue2.vhdl -e
synth_failure issue3.vhdl -e
synth_failure issue4.vhdl -e

clean

echo "Test successful"
