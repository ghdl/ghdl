#! /bin/sh

. ../../testenv.sh

synth_analyze ent
clean

synth_failure smod0.vhdl -e
synth_failure srem0.vhdl -e
synth_failure sdiv0.vhdl -e
synth_failure urem0.vhdl -e
synth_failure udiv0.vhdl -e

echo "Test successful"
