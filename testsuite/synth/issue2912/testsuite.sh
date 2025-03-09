#! /bin/sh

. ../../testenv.sh

synth --out=verilog top.vhdl -e > syn_top.v

echo "Test successful"
