#! /bin/sh

. ../../testenv.sh

synth alu.vhdl -e $t > syn_alu.vhdl
analyze syn_alu.vhdl
clean

echo "Test successful"
