#! /bin/sh

. ../../testenv.sh

synth barrel_shifter.vhdl -e $t > syn_barrel_shifter.vhdl
analyze syn_barrel_shifter.vhdl
clean

echo "Test successful"
