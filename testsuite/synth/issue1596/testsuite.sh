#! /bin/sh

. ../../testenv.sh

synth ent.vhdl ent_working.vhdl -e > syn_working.vhdl
synth ent.vhdl ent_bug.vhdl -e > syn_bug.vhdl

echo "Test successful"
