#! /bin/sh

. ../../testenv.sh

synth_failure bugr.vhdl -e
synth_failure bugw.vhdl -e
synth_failure --latches bug.vhdl -e

echo "Test successful"
