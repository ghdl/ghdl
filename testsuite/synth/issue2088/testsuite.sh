#! /bin/sh

. ../../testenv.sh

synth_only bug
synth_only bug2
synth_failure bug3.vhdl -e

echo "Test successful"
