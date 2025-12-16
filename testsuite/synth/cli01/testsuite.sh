#! /bin/sh

. ../../testenv.sh

synth -dm -de dpram1.vhdl -e > syn_dpram1.vhdl
synth --out=verilog -dm -de dpram1.vhdl -e > syn_dpram1.v
synth --stats -o=syn_dpram1.vhdl dpram1.vhdl -e

echo "Test successful"
