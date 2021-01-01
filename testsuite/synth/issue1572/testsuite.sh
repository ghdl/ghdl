#! /bin/sh

. ../../testenv.sh

synth --std=08 ent.vhdl ent.psl -e > syn_ent.vhdl

echo "Test successful"
