#! /bin/sh

. ../../testenv.sh

synth slv_negation.vhdl -e slv_negation > syn_slv_negation.vhdl

echo "Test successful"
