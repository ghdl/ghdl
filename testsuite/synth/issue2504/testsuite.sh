#! /bin/sh

. ../../testenv.sh

synth_tb shift1

synth_failure repro2.vhdl -e
synth_failure repro3.vhdl -e

echo "Test successful"
