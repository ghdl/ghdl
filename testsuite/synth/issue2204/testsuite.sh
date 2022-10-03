#! /bin/sh

. ../../testenv.sh

synth_failure -fpsl crash.vhdl -e
synth_failure -fpsl crash2.vhdl -e
synth_failure -fpsl crash3.vhdl -e
synth_failure -fpsl crash4.vhdl -e
synth_failure -fpsl crash5.vhdl -e

echo "Test successful"
