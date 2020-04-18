#! /bin/sh

. ../../testenv.sh

synth_failure DSPn.vhdl -e
synth_failure issue.vhdl -e

echo "Test successful"
