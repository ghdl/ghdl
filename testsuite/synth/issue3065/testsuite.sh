#! /bin/sh

. ../../testenv.sh

synth top.vhdl -e > syn_top.vhdl
grep distributed syn_top.vhdl

analyze syn_top.vhdl
clean

echo "Test successful"
