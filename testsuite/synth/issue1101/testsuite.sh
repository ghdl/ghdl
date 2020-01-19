#! /bin/sh

. ../../testenv.sh

synth --std=08 ent.vhdl -e > syn_ent.vhdl

echo "Test successful"
