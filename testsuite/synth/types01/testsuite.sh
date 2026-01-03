#! /bin/sh

. ../../testenv.sh

synth_failure float01.vhdl -e
synth_failure float02.vhdl -e
synth_only file01
synth_only access01

echo "Test successful"
