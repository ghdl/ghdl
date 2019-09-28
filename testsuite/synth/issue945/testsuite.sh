#! /bin/sh

. ../../testenv.sh

synth ent.vhdl -e ent > syn_ent.vhdl
clean

echo "Test successful"
