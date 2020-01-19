#! /bin/sh

. ../../testenv.sh

synth top.vhdl -e > syn_top.vhdl

echo "Test successful"
