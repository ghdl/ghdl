#! /bin/sh

. ../../testenv.sh

synth ent.vhdl -e > syn_ent.vhdl

echo "Test successful"
