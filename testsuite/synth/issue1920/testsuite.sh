#! /bin/sh

. ../../testenv.sh

synth_failure ent1.vhdl -e
synth_failure ent2.vhdl -e

echo "Test successful"
