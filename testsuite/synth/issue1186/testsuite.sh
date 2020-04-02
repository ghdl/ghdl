#! /bin/sh

. ../../testenv.sh

synth ent.vhdl -e > syn_ent.vhdl
clean

echo "Test successful"
