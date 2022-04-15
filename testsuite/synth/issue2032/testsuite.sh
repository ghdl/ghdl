#! /bin/sh

. ../../testenv.sh

synth_failure repro1.vhdl -e
synth_failure repro2.vhdl -e
synth_failure repro3.vhdl -e
synth_failure repro4.vhdl -e

echo "Test successful"
