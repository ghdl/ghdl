#! /bin/sh

. ../../testenv.sh

synth_tb ent2

synth ent.vhdl -e > syn_ent.vhdl

echo "Test successful"
