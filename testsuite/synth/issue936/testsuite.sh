#! /bin/sh

. ../../testenv.sh

synth ent.vhdl -e ent > syn_ent.vhdl

echo "Test successful"
