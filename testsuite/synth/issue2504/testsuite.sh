#! /bin/sh

. ../../testenv.sh

synth_tb shift1

synth_failure repro2.vhdl -e
synth_failure repro3.vhdl -e

synth_only nomem1
synth_only nomem2
synth_only nomem3

echo "Test successful"
