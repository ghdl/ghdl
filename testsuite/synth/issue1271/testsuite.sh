#! /bin/sh

. ../../testenv.sh

synth_failure issue.vhdl -e
synth_failure issue2.vhdl -e
synth_failure issue3.vhdl -e

echo "Test successful"
